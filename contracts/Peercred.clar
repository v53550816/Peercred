

;; token definitions

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PAPER_NOT_FOUND (err u101))
(define-constant ERR_REVIEW_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_REVIEWED (err u103))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u104))
(define-constant ERR_INVALID_SCORE (err u105))
(define-constant ERR_PAPER_ALREADY_EXISTS (err u106))
(define-constant ERR_VOTING_CLOSED (err u107))
(define-constant ERR_ALREADY_VOTED (err u108))
(define-constant MIN_REPUTATION_TO_REVIEW u50)
(define-constant REVIEW_REWARD u10)
(define-constant VOTING_PERIOD u144)

;; data vars
(define-data-var next-paper-id uint u1)
(define-data-var next-review-id uint u1)

;; data maps
(define-map papers
  { paper-id: uint }
  {
    author: principal,
    title: (string-ascii 256),
    content-hash: (string-ascii 64),
    submission-block: uint,
    status: (string-ascii 20),
    total-reviews: uint,
    average-score: uint
  }
)

(define-map reviews
  { review-id: uint }
  {
    paper-id: uint,
    reviewer: principal,
    score: uint,
    content-hash: (string-ascii 64),
    review-block: uint,
    helpful-votes: uint,
    total-votes: uint
  }
)

(define-map user-reputation
  { user: principal }
  { reputation: uint }
)

(define-map paper-reviews
  { paper-id: uint, reviewer: principal }
  { review-id: uint }
)

(define-map review-votes
  { review-id: uint, voter: principal }
  { vote: bool }
)

(define-map author-papers
  { author: principal }
  { paper-count: uint }
)

(define-map reviewer-stats
  { reviewer: principal }
  { reviews-count: uint, total-score-given: uint }
)

;; public functions
(define-public (submit-paper (title (string-ascii 256)) (content-hash (string-ascii 64)))
  (let
    (
      (paper-id (var-get next-paper-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq (len title) (len title)) ERR_PAPER_ALREADY_EXISTS)
    (map-set papers
      { paper-id: paper-id }
      {
        author: tx-sender,
        title: title,
        content-hash: content-hash,
        submission-block: current-block,
        status: "pending",
        total-reviews: u0,
        average-score: u0
      }
    )
    (map-set author-papers
      { author: tx-sender }
      { paper-count: (+ (get-author-paper-count tx-sender) u1) }
    )
    (var-set next-paper-id (+ paper-id u1))
    (ok paper-id)
  )
)

(define-public (submit-review (paper-id uint) (score uint) (content-hash (string-ascii 64)))
  (let
    (
      (reviewer-rep (get-user-reputation tx-sender))
      (review-id (var-get next-review-id))
      (paper-data (unwrap! (map-get? papers { paper-id: paper-id }) ERR_PAPER_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (>= reviewer-rep MIN_REPUTATION_TO_REVIEW) ERR_INSUFFICIENT_REPUTATION)
    (asserts! (and (>= score u1) (<= score u10)) ERR_INVALID_SCORE)
    (asserts! (is-none (map-get? paper-reviews { paper-id: paper-id, reviewer: tx-sender })) ERR_ALREADY_REVIEWED)
    (asserts! (not (is-eq tx-sender (get author paper-data))) ERR_NOT_AUTHORIZED)
    
    (map-set reviews
      { review-id: review-id }
      {
        paper-id: paper-id,
        reviewer: tx-sender,
        score: score,
        content-hash: content-hash,
        review-block: current-block,
        helpful-votes: u0,
        total-votes: u0
      }
    )
    
    (map-set paper-reviews
      { paper-id: paper-id, reviewer: tx-sender }
      { review-id: review-id }
    )
    
    (let
      (
        (new-total-reviews (+ (get total-reviews paper-data) u1))
        (current-total-score (* (get average-score paper-data) (get total-reviews paper-data)))
        (new-average-score (/ (+ current-total-score score) new-total-reviews))
      )
      (map-set papers
        { paper-id: paper-id }
        (merge paper-data {
          total-reviews: new-total-reviews,
          average-score: new-average-score,
          status: (if (>= new-total-reviews u3) "reviewed" "pending")
        })
      )
    )
    
    (map-set user-reputation
      { user: tx-sender }
      { reputation: (+ reviewer-rep REVIEW_REWARD) }
    )
    
    (map-set reviewer-stats
      { reviewer: tx-sender }
      {
        reviews-count: (+ (get-reviewer-review-count tx-sender) u1),
        total-score-given: (+ (get-reviewer-total-score tx-sender) score)
      }
    )
    
    (var-set next-review-id (+ review-id u1))
    (ok review-id)
  )
)

(define-public (vote-on-review (review-id uint) (helpful bool))
  (let
    (
      (review-data (unwrap! (map-get? reviews { review-id: review-id }) ERR_REVIEW_NOT_FOUND))
      (voter-rep (get-user-reputation tx-sender))
      (current-block stacks-block-height)
      (review-block (get review-block review-data))
    )
    (asserts! (>= voter-rep u25) ERR_INSUFFICIENT_REPUTATION)
    (asserts! (<= (- current-block review-block) VOTING_PERIOD) ERR_VOTING_CLOSED)
    (asserts! (is-none (map-get? review-votes { review-id: review-id, voter: tx-sender })) ERR_ALREADY_VOTED)
    
    (map-set review-votes
      { review-id: review-id, voter: tx-sender }
      { vote: helpful }
    )
    
    (let
      (
        (new-total-votes (+ (get total-votes review-data) u1))
        (new-helpful-votes (if helpful (+ (get helpful-votes review-data) u1) (get helpful-votes review-data)))
      )
      (map-set reviews
        { review-id: review-id }
        (merge review-data {
          helpful-votes: new-helpful-votes,
          total-votes: new-total-votes
        })
      )
      
      (if (and helpful (> new-total-votes u2))
        (map-set user-reputation
          { user: (get reviewer review-data) }
          { reputation: (+ (get-user-reputation (get reviewer review-data)) u5) }
        )
        true
      )
    )
    (ok true)
  )
)

(define-public (initialize-reputation (user principal) (initial-rep uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set user-reputation { user: user } { reputation: initial-rep })
    (ok true)
  )
)

;; read only functions
(define-read-only (get-paper (paper-id uint))
  (map-get? papers { paper-id: paper-id })
)

(define-read-only (get-review (review-id uint))
  (map-get? reviews { review-id: review-id })
)

(define-read-only (get-user-reputation (user principal))
  (default-to u0 (get reputation (map-get? user-reputation { user: user })))
)

(define-read-only (get-paper-review (paper-id uint) (reviewer principal))
  (map-get? paper-reviews { paper-id: paper-id, reviewer: reviewer })
)

(define-read-only (get-review-vote (review-id uint) (voter principal))
  (map-get? review-votes { review-id: review-id, voter: voter })
)

(define-read-only (get-author-paper-count (author principal))
  (default-to u0 (get paper-count (map-get? author-papers { author: author })))
)

(define-read-only (get-reviewer-review-count (reviewer principal))
  (default-to u0 (get reviews-count (map-get? reviewer-stats { reviewer: reviewer })))
)

(define-read-only (get-reviewer-total-score (reviewer principal))
  (default-to u0 (get total-score-given (map-get? reviewer-stats { reviewer: reviewer })))
)

(define-read-only (get-next-paper-id)
  (var-get next-paper-id)
)

(define-read-only (get-next-review-id)
  (var-get next-review-id)
)

(define-read-only (get-paper-status (paper-id uint))
  (match (get-paper paper-id)
    paper-data (some (get status paper-data))
    none
  )
)

(define-read-only (calculate-reviewer-average-score (reviewer principal))
  (let
    (
      (review-count (get-reviewer-review-count reviewer))
      (total-score (get-reviewer-total-score reviewer))
    )
    (if (> review-count u0)
      (some (/ total-score review-count))
      none
    )
  )
)

(define-read-only (get-review-helpfulness-ratio (review-id uint))
  (match (get-review review-id)
    review-data 
      (let
        (
          (helpful (get helpful-votes review-data))
          (total (get total-votes review-data))
        )
        (if (> total u0)
          (some (/ (* helpful u100) total))
          (some u0)
        )
      )
    none
  )
)

;; private functions