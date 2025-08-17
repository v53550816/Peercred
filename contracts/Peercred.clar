

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

(define-constant ERR_COLLABORATION_NOT_FOUND (err u109))
(define-constant ERR_NOT_COLLABORATOR (err u110))
(define-constant ERR_INVITATION_EXPIRED (err u111))
(define-constant ERR_INVITATION_NOT_FOUND (err u112))
(define-constant ERR_ALREADY_COLLABORATOR (err u113))
(define-constant ERR_INVALID_CONTRIBUTION_WEIGHT (err u114))
(define-constant ERR_COLLABORATION_FINALIZED (err u115))
(define-constant ERR_INSUFFICIENT_COLLABORATORS (err u116))
(define-constant ERR_NOT_COLLABORATION_LEADER (err u117))
(define-constant COLLABORATION_INVITATION_PERIOD u1008)
(define-constant MAX_COLLABORATORS u10)
(define-constant MIN_COLLABORATORS_FOR_PAPER u2)

(define-data-var next-collaboration-id uint u1)
(define-data-var next-invitation-id uint u1)

(define-map collaborations
  { collaboration-id: uint }
  {
    leader: principal,
    title: (string-ascii 128),
    description: (string-ascii 512),
    creation-block: uint,
    status: (string-ascii 20),
    total-collaborators: uint,
    finalized: bool,
    associated-paper-id: (optional uint)
  }
)

(define-map collaboration-members
  { collaboration-id: uint, member: principal }
  {
    contribution-weight: uint,
    join-block: uint,
    role: (string-ascii 32),
    active: bool
  }
)

(define-map collaboration-invitations
  { invitation-id: uint }
  {
    collaboration-id: uint,
    inviter: principal,
    invitee: principal,
    invitation-block: uint,
    status: (string-ascii 20),
    message: (string-ascii 256)
  }
)

(define-map user-collaborations
  { user: principal }
  { active-collaborations: uint, total-collaborations: uint }
)

(define-map collaboration-papers
  { collaboration-id: uint }
  { paper-id: uint, submitted: bool }
)

(define-map collaboration-reputation-pool
  { collaboration-id: uint }
  { total-reputation-earned: uint, distributed: bool }
)

(define-public (create-collaboration (title (string-ascii 128)) (description (string-ascii 512)))
  (let
    (
      (collaboration-id (var-get next-collaboration-id))
      (current-block stacks-block-height)
    )
    (map-set collaborations
      { collaboration-id: collaboration-id }
      {
        leader: tx-sender,
        title: title,
        description: description,
        creation-block: current-block,
        status: "forming",
        total-collaborators: u1,
        finalized: false,
        associated-paper-id: none
      }
    )
    
    (map-set collaboration-members
      { collaboration-id: collaboration-id, member: tx-sender }
      {
        contribution-weight: u100,
        join-block: current-block,
        role: "leader",
        active: true
      }
    )
    
    (map-set user-collaborations
      { user: tx-sender }
      {
        active-collaborations: (+ (get-user-active-collaborations tx-sender) u1),
        total-collaborations: (+ (get-user-total-collaborations tx-sender) u1)
      }
    )
    
    (var-set next-collaboration-id (+ collaboration-id u1))
    (ok collaboration-id)
  )
)

(define-public (invite-collaborator (collaboration-id uint) (invitee principal) (message (string-ascii 256)))
  (let
    (
      (collaboration-data (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) ERR_COLLABORATION_NOT_FOUND))
      (invitation-id (var-get next-invitation-id))
      (current-block stacks-block-height)
    )
    (asserts! (or (is-eq tx-sender (get leader collaboration-data)) 
                  (is-some (map-get? collaboration-members { collaboration-id: collaboration-id, member: tx-sender }))) 
              ERR_NOT_COLLABORATOR)
    (asserts! (not (get finalized collaboration-data)) ERR_COLLABORATION_FINALIZED)
    (asserts! (< (get total-collaborators collaboration-data) MAX_COLLABORATORS) ERR_NOT_AUTHORIZED)
    (asserts! (is-none (map-get? collaboration-members { collaboration-id: collaboration-id, member: invitee })) 
              ERR_ALREADY_COLLABORATOR)
    
    (map-set collaboration-invitations
      { invitation-id: invitation-id }
      {
        collaboration-id: collaboration-id,
        inviter: tx-sender,
        invitee: invitee,
        invitation-block: current-block,
        status: "pending",
        message: message
      }
    )
    
    (var-set next-invitation-id (+ invitation-id u1))
    (ok invitation-id)
  )
)

(define-public (accept-invitation (invitation-id uint) (contribution-weight uint))
  (let
    (
      (invitation-data (unwrap! (map-get? collaboration-invitations { invitation-id: invitation-id }) ERR_INVITATION_NOT_FOUND))
      (collaboration-id (get collaboration-id invitation-data))
      (collaboration-data (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) ERR_COLLABORATION_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get invitee invitation-data)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq "pending" (get status invitation-data)) ERR_NOT_AUTHORIZED)
    (asserts! (<= (- current-block (get invitation-block invitation-data)) COLLABORATION_INVITATION_PERIOD) 
              ERR_INVITATION_EXPIRED)
    (asserts! (and (>= contribution-weight u1) (<= contribution-weight u100)) ERR_INVALID_CONTRIBUTION_WEIGHT)
    (asserts! (not (get finalized collaboration-data)) ERR_COLLABORATION_FINALIZED)
    
    (map-set collaboration-invitations
      { invitation-id: invitation-id }
      (merge invitation-data { status: "accepted" })
    )
    
    (map-set collaboration-members
      { collaboration-id: collaboration-id, member: tx-sender }
      {
        contribution-weight: contribution-weight,
        join-block: current-block,
        role: "collaborator",
        active: true
      }
    )
    
    (map-set collaborations
      { collaboration-id: collaboration-id }
      (merge collaboration-data { total-collaborators: (+ (get total-collaborators collaboration-data) u1) })
    )
    
    (map-set user-collaborations
      { user: tx-sender }
      {
        active-collaborations: (+ (get-user-active-collaborations tx-sender) u1),
        total-collaborations: (+ (get-user-total-collaborations tx-sender) u1)
      }
    )
    
    (ok true)
  )
)

(define-public (finalize-collaboration (collaboration-id uint))
  (let
    (
      (collaboration-data (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) ERR_COLLABORATION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get leader collaboration-data)) ERR_NOT_COLLABORATION_LEADER)
    (asserts! (>= (get total-collaborators collaboration-data) MIN_COLLABORATORS_FOR_PAPER) 
              ERR_INSUFFICIENT_COLLABORATORS)
    (asserts! (not (get finalized collaboration-data)) ERR_COLLABORATION_FINALIZED)
    
    (map-set collaborations
      { collaboration-id: collaboration-id }
      (merge collaboration-data { 
        status: "active",
        finalized: true
      })
    )
    
    (ok true)
  )
)

(define-public (submit-collaborative-paper (collaboration-id uint) (title (string-ascii 256)) (content-hash (string-ascii 64)))
  (let
    (
      (collaboration-data (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) ERR_COLLABORATION_NOT_FOUND))
      (paper-id (var-get next-paper-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get leader collaboration-data)) ERR_NOT_COLLABORATION_LEADER)
    (asserts! (get finalized collaboration-data) ERR_NOT_AUTHORIZED)
    (asserts! (is-none (get associated-paper-id collaboration-data)) ERR_PAPER_ALREADY_EXISTS)
    
    (map-set papers
      { paper-id: paper-id }
      {
        author: tx-sender,
        title: title,
        content-hash: content-hash,
        submission-block: current-block,
        status: "collaborative",
        total-reviews: u0,
        average-score: u0
      }
    )
    
    (map-set collaborations
      { collaboration-id: collaboration-id }
      (merge collaboration-data { associated-paper-id: (some paper-id) })
    )
    
    (map-set collaboration-papers
      { collaboration-id: collaboration-id }
      { paper-id: paper-id, submitted: true }
    )
    
    (map-set author-papers
      { author: tx-sender }
      { paper-count: (+ (get-author-paper-count tx-sender) u1) }
    )
    
    (var-set next-paper-id (+ paper-id u1))
    (ok paper-id)
  )
)

(define-public (distribute-collaboration-reputation (collaboration-id uint))
  (let
    (
      (collaboration-data (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) ERR_COLLABORATION_NOT_FOUND))
      (paper-id-opt (get associated-paper-id collaboration-data))
      (paper-id (unwrap! paper-id-opt ERR_PAPER_NOT_FOUND))
      (paper-data (unwrap! (map-get? papers { paper-id: paper-id }) ERR_PAPER_NOT_FOUND))
      (reputation-pool-data (default-to { total-reputation-earned: u0, distributed: false } 
                                       (map-get? collaboration-reputation-pool { collaboration-id: collaboration-id })))
    )
    (asserts! (is-eq tx-sender (get leader collaboration-data)) ERR_NOT_COLLABORATION_LEADER)
    (asserts! (is-eq "reviewed" (get status paper-data)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get distributed reputation-pool-data)) ERR_NOT_AUTHORIZED)
    
    (let
      (
        (total-reputation (* (get average-score paper-data) (get total-collaborators collaboration-data)))
        (leader-member-data (unwrap! (map-get? collaboration-members { collaboration-id: collaboration-id, member: tx-sender }) 
                                   ERR_NOT_COLLABORATOR))
        (leader-share (/ (* total-reputation (get contribution-weight leader-member-data)) u100))
      )
      (map-set user-reputation
        { user: tx-sender }
        { reputation: (+ (get-user-reputation tx-sender) leader-share) }
      )
      
      (map-set collaboration-reputation-pool
        { collaboration-id: collaboration-id }
        { total-reputation-earned: total-reputation, distributed: true }
      )
    )
    (ok true)
  )
)

(define-read-only (get-collaboration (collaboration-id uint))
  (map-get? collaborations { collaboration-id: collaboration-id })
)

(define-read-only (get-collaboration-member (collaboration-id uint) (member principal))
  (map-get? collaboration-members { collaboration-id: collaboration-id, member: member })
)

(define-read-only (get-collaboration-invitation (invitation-id uint))
  (map-get? collaboration-invitations { invitation-id: invitation-id })
)

(define-read-only (get-user-active-collaborations (user principal))
  (default-to u0 (get active-collaborations (map-get? user-collaborations { user: user })))
)

(define-read-only (get-user-total-collaborations (user principal))
  (default-to u0 (get total-collaborations (map-get? user-collaborations { user: user })))
)

(define-read-only (get-collaboration-paper (collaboration-id uint))
  (map-get? collaboration-papers { collaboration-id: collaboration-id })
)

(define-read-only (get-collaboration-reputation-pool (collaboration-id uint))
  (map-get? collaboration-reputation-pool { collaboration-id: collaboration-id })
)

(define-read-only (is-collaboration-member (collaboration-id uint) (user principal))
  (is-some (map-get? collaboration-members { collaboration-id: collaboration-id, member: user }))
)

(define-read-only (get-next-collaboration-id)
  (var-get next-collaboration-id)
)

(define-read-only (get-next-invitation-id)
  (var-get next-invitation-id)
)

(define-constant ERR_CITATION_NOT_FOUND (err u118))
(define-constant ERR_SELF_CITATION_NOT_ALLOWED (err u119))
(define-constant ERR_CITATION_ALREADY_EXISTS (err u120))
(define-constant ERR_INVALID_CITATION_TYPE (err u121))
(define-constant ERR_PAPER_NOT_PUBLISHED (err u122))
(define-constant CITATION_REPUTATION_REWARD u5)
(define-constant H_INDEX_CALCULATION_THRESHOLD u10)
(define-constant MAX_CITATIONS_PER_PAPER u50)

(define-data-var next-citation-id uint u1)

(define-map paper-citations
  { citing-paper-id: uint, cited-paper-id: uint }
  {
    citation-id: uint,
    citation-type: (string-ascii 20),
    citation-block: uint,
    verified: bool
  }
)

(define-map citation-details
  { citation-id: uint }
  {
    citing-paper-id: uint,
    cited-paper-id: uint,
    citing-author: principal,
    cited-author: principal,
    citation-context: (string-ascii 256),
    importance-score: uint
  }
)

(define-map paper-citation-counts
  { paper-id: uint }
  {
    incoming-citations: uint,
    outgoing-citations: uint,
    impact-score: uint,
    last-citation-block: uint
  }
)

(define-map author-citation-metrics
  { author: principal }
  {
    total-citations-received: uint,
    total-citations-given: uint,
    h-index: uint,
    citation-reputation: uint,
    most-cited-paper-id: (optional uint)
  }
)

(define-map citation-networks
  { citing-author: principal, cited-author: principal }
  {
    citation-count: uint,
    first-citation-block: uint,
    latest-citation-block: uint,
    collaboration-strength: uint
  }
)

(define-map paper-impact-history
  { paper-id: uint }
  {
    citation-milestones: (list 10 uint),
    peak-citation-period: uint,
    sustained-impact-score: uint
  }
)

(define-public (add-paper-citation (citing-paper-id uint) (cited-paper-id uint) (citation-type (string-ascii 20)) (citation-context (string-ascii 256)) (importance-score uint))
  (let
    (
      (citing-paper (unwrap! (map-get? papers { paper-id: citing-paper-id }) ERR_PAPER_NOT_FOUND))
      (cited-paper (unwrap! (map-get? papers { paper-id: cited-paper-id }) ERR_PAPER_NOT_FOUND))
      (citation-id (var-get next-citation-id))
      (current-block stacks-block-height)
      (citing-author (get author citing-paper))
      (cited-author (get author cited-paper))
    )
    (asserts! (is-eq tx-sender citing-author) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq citing-paper-id cited-paper-id)) ERR_SELF_CITATION_NOT_ALLOWED)
    (asserts! (or (is-eq "reviewed" (get status citing-paper)) (is-eq "collaborative" (get status citing-paper))) ERR_PAPER_NOT_PUBLISHED)
    (asserts! (or (is-eq "reviewed" (get status cited-paper)) (is-eq "collaborative" (get status cited-paper))) ERR_PAPER_NOT_PUBLISHED)
    (asserts! (is-none (map-get? paper-citations { citing-paper-id: citing-paper-id, cited-paper-id: cited-paper-id })) ERR_CITATION_ALREADY_EXISTS)
    (asserts! (and (>= importance-score u1) (<= importance-score u10)) ERR_INVALID_SCORE)
    (asserts! (< (get-paper-outgoing-citations citing-paper-id) MAX_CITATIONS_PER_PAPER) ERR_NOT_AUTHORIZED)
    
    (map-set paper-citations
      { citing-paper-id: citing-paper-id, cited-paper-id: cited-paper-id }
      {
        citation-id: citation-id,
        citation-type: citation-type,
        citation-block: current-block,
        verified: true
      }
    )
    
    (map-set citation-details
      { citation-id: citation-id }
      {
        citing-paper-id: citing-paper-id,
        cited-paper-id: cited-paper-id,
        citing-author: citing-author,
        cited-author: cited-author,
        citation-context: citation-context,
        importance-score: importance-score
      }
    )
    
    (let
      (
        (cited-paper-counts (default-to { incoming-citations: u0, outgoing-citations: u0, impact-score: u0, last-citation-block: u0 }
                                        (map-get? paper-citation-counts { paper-id: cited-paper-id })))
        (citing-paper-counts (default-to { incoming-citations: u0, outgoing-citations: u0, impact-score: u0, last-citation-block: u0 }
                                         (map-get? paper-citation-counts { paper-id: citing-paper-id })))
        (new-cited-incoming (+ (get incoming-citations cited-paper-counts) u1))
        (new-citing-outgoing (+ (get outgoing-citations citing-paper-counts) u1))
        (new-impact-score (+ (get impact-score cited-paper-counts) importance-score))
      )
      (map-set paper-citation-counts
        { paper-id: cited-paper-id }
        (merge cited-paper-counts {
          incoming-citations: new-cited-incoming,
          impact-score: new-impact-score,
          last-citation-block: current-block
        })
      )
      
      (map-set paper-citation-counts
        { paper-id: citing-paper-id }
        (merge citing-paper-counts {
          outgoing-citations: new-citing-outgoing
        })
      )
    )
    
    (let
      (
        (cited-author-metrics (default-to { total-citations-received: u0, total-citations-given: u0, h-index: u0, citation-reputation: u0, most-cited-paper-id: none }
                                          (map-get? author-citation-metrics { author: cited-author })))
        (citing-author-metrics (default-to { total-citations-received: u0, total-citations-given: u0, h-index: u0, citation-reputation: u0, most-cited-paper-id: none }
                                           (map-get? author-citation-metrics { author: citing-author })))
        (new-cited-total (+ (get total-citations-received cited-author-metrics) u1))
        (new-citing-total (+ (get total-citations-given citing-author-metrics) u1))
      )
      (map-set author-citation-metrics
        { author: cited-author }
        (merge cited-author-metrics {
          total-citations-received: new-cited-total,
          citation-reputation: (+ (get citation-reputation cited-author-metrics) CITATION_REPUTATION_REWARD)
        })
      )
      
      (map-set author-citation-metrics
        { author: citing-author }
        (merge citing-author-metrics {
          total-citations-given: new-citing-total
        })
      )
    )
    
    (map-set user-reputation
      { user: cited-author }
      { reputation: (+ (get-user-reputation cited-author) CITATION_REPUTATION_REWARD) }
    )
    
    (let
      (
        (network-data (default-to { citation-count: u0, first-citation-block: current-block, latest-citation-block: current-block, collaboration-strength: u0 }
                                  (map-get? citation-networks { citing-author: citing-author, cited-author: cited-author })))
        (new-count (+ (get citation-count network-data) u1))
        (new-strength (+ (get collaboration-strength network-data) importance-score))
      )
      (map-set citation-networks
        { citing-author: citing-author, cited-author: cited-author }
        {
          citation-count: new-count,
          first-citation-block: (get first-citation-block network-data),
          latest-citation-block: current-block,
          collaboration-strength: new-strength
        }
      )
    )
    
    (var-set next-citation-id (+ citation-id u1))
    (ok citation-id)
  )
)

(define-public (update-author-h-index (author principal))
  (let
    (
      (author-metrics (default-to { total-citations-received: u0, total-citations-given: u0, h-index: u0, citation-reputation: u0, most-cited-paper-id: none }
                                  (map-get? author-citation-metrics { author: author })))
      (paper-count (get-author-paper-count author))
    )
    (asserts! (>= paper-count H_INDEX_CALCULATION_THRESHOLD) ERR_INSUFFICIENT_REPUTATION)
    
    (let
      (
        (calculated-h-index (calculate-h-index-for-author author))
      )
      (map-set author-citation-metrics
        { author: author }
        (merge author-metrics { h-index: calculated-h-index })
      )
      (ok calculated-h-index)
    )
  )
)

(define-public (verify-citation (citation-id uint))
  (let
    (
      (citation-data (unwrap! (map-get? citation-details { citation-id: citation-id }) ERR_CITATION_NOT_FOUND))
      (citing-paper-id (get citing-paper-id citation-data))
      (cited-paper-id (get cited-paper-id citation-data))
    )
    (asserts! (or (is-eq tx-sender (get cited-author citation-data)) (is-eq tx-sender CONTRACT_OWNER)) ERR_NOT_AUTHORIZED)
    
    (map-set paper-citations
      { citing-paper-id: citing-paper-id, cited-paper-id: cited-paper-id }
      (merge (unwrap! (map-get? paper-citations { citing-paper-id: citing-paper-id, cited-paper-id: cited-paper-id }) ERR_CITATION_NOT_FOUND)
             { verified: true })
    )
    (ok true)
  )
)

(define-read-only (get-paper-citation (citing-paper-id uint) (cited-paper-id uint))
  (map-get? paper-citations { citing-paper-id: citing-paper-id, cited-paper-id: cited-paper-id })
)

(define-read-only (get-citation-details (citation-id uint))
  (map-get? citation-details { citation-id: citation-id })
)

(define-read-only (get-paper-citation-counts (paper-id uint))
  (map-get? paper-citation-counts { paper-id: paper-id })
)

(define-read-only (get-author-citation-metrics (author principal))
  (map-get? author-citation-metrics { author: author })
)

(define-read-only (get-citation-network (citing-author principal) (cited-author principal))
  (map-get? citation-networks { citing-author: citing-author, cited-author: cited-author })
)

(define-read-only (get-paper-impact-history (paper-id uint))
  (map-get? paper-impact-history { paper-id: paper-id })
)

(define-read-only (get-paper-incoming-citations (paper-id uint))
  (default-to u0 (get incoming-citations (map-get? paper-citation-counts { paper-id: paper-id })))
)

(define-read-only (get-paper-outgoing-citations (paper-id uint))
  (default-to u0 (get outgoing-citations (map-get? paper-citation-counts { paper-id: paper-id })))
)

(define-read-only (get-paper-impact-score (paper-id uint))
  (default-to u0 (get impact-score (map-get? paper-citation-counts { paper-id: paper-id })))
)

(define-read-only (get-author-h-index (author principal))
  (default-to u0 (get h-index (map-get? author-citation-metrics { author: author })))
)

(define-read-only (get-author-citation-reputation (author principal))
  (default-to u0 (get citation-reputation (map-get? author-citation-metrics { author: author })))
)

(define-read-only (calculate-h-index-for-author (author principal))
  (let
    (
      (total-papers (get-author-paper-count author))
      (total-citations (default-to u0 (get total-citations-received (map-get? author-citation-metrics { author: author }))))
      (citation-factor (/ total-citations u2))
    )
    (if (> total-papers u0)
      (if (< total-papers citation-factor) total-papers citation-factor)
      u0
    )
  )
)

(define-read-only (get-next-citation-id)
  (var-get next-citation-id)
)

(define-read-only (is-citation-verified (citing-paper-id uint) (cited-paper-id uint))
  (match (get-paper-citation citing-paper-id cited-paper-id)
    citation-data (get verified citation-data)
    false
  )
)

;; private functions


