# 🧬 Peercred - Science Peer Review DAO

A decentralized platform for scientific paper peer review with reputation-based validation system built on Stacks blockchain.

## 🌟 Features

- **📄 Paper Submission**: Scientists can submit papers with content hashes for review
- **👥 Peer Review System**: Qualified reviewers can evaluate papers and provide scores (1-10)
- **🏆 Reputation System**: Users earn reputation points for quality reviews and participation
- **🗳️ Review Voting**: Community can vote on review helpfulness to maintain quality
- **📊 Statistics Tracking**: Comprehensive stats for authors, reviewers, and papers
- **⏰ Time-based Voting**: Limited voting periods to ensure timely feedback

## 🚀 Getting Started

### Prerequisites

- Clarinet CLI installed
- Stacks wallet for testing

### Installation

1. Clone the repository
2. Navigate to the project directory
3. Deploy the contract using Clarinet

```bash
clarinet deploy
```

## 📖 Usage

### For Authors 📝

**Submit a Paper:**
```clarity
(contract-call? .Peercred submit-paper "Paper Title" "content-hash-here")
```

### For Reviewers 🔍

**Submit a Review:**
```clarity
(contract-call? .Peercred submit-review u1 u8 "review-content-hash")
```
- Paper ID: `u1`
- Score: `u8` (1-10 scale)
- Requires minimum 50 reputation points

### For Community Members 🌐

**Vote on Review Quality:**
```clarity
(contract-call? .Peercred vote-on-review u1 true)
```
- Review ID: `u1`
- Helpful: `true/false`
- Requires minimum 25 reputation points

## 🎯 Key Functions

### Public Functions

| Function | Description | Requirements |
|----------|-------------|--------------|
| `submit-paper` | Submit a new paper for review | None |
| `submit-review` | Review a submitted paper | 50+ reputation |
| `vote-on-review` | Vote on review helpfulness | 25+ reputation |
| `initialize-reputation` | Set initial reputation (owner only) | Contract owner |

### Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-paper` | Get paper details by ID |
| `get-review` | Get review details by ID |
| `get-user-reputation` | Get user's reputation score |
| `get-paper-status` | Get current paper status |
| `calculate-reviewer-average-score` | Get reviewer's average score |

## 🏅 Reputation System

- **📈 Earn Reputation**: +10 points for each review submitted
- **⭐ Bonus Points**: +5 points for helpful reviews (voted by community)
- **🎯 Minimum Requirements**:
  - 50 reputation to submit reviews
  - 25 reputation to vote on reviews

## 📊 Paper Lifecycle

1. **Pending** → Paper submitted, awaiting reviews
2. **Reviewed** → Paper has 3+ reviews and final status

## 🔧 Configuration

- **Voting Period**: 144 blocks (~24 hours)
- **Review Reward**: 10 reputation points
- **Minimum Review Reputation**: 50 points
- **Minimum Voting Reputation**: 25 points

## 🛡️ Security Features

- Authors cannot review their own papers
- One review per reviewer per paper
- Time-limited voting periods
- Reputation-gated participation

## 📈 Analytics

Track comprehensive statistics including:
- Author paper counts
- Reviewer performance metrics
- Review helpfulness ratios
- Average scores and reputation trends

## 🤝 Contributing

Contributions are welcome! Please ensure all code follows the existing patterns and includes appropriate tests.

## 📄 License

This project is open source and available under the MIT License.

---


