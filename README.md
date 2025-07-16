# 🚜 On-chain Tool Rental Agreements

A decentralized solution for managing agricultural tool rentals using smart contracts on the Stacks blockchain.

## 🎯 Overview

This smart contract enables secure peer-to-peer tool rentals with automated deposit management and penalty enforcement. Perfect for agricultural communities looking to share resources efficiently and safely.

## ✨ Features

- 🛠️ Tool registration with customizable rates and deposit amounts
- 💰 Automated payment and deposit handling
- ⏱️ Time-based rental agreements
- 🔄 Automated deposit returns with penalty deductions
- 📊 Transparent rental history and tool availability

## 📝 Contract Functions

### For Tool Owners

- `register-tool`: Register a new tool with rental terms
- `get-tool-info`: View tool details and availability

### For Renters

- `rent-tool`: Rent a tool by paying deposit and fees
- `return-tool`: Return tool and receive deposit (minus any penalties)
- `get-rental-info`: Check current rental status

## 🚀 Getting Started

1. Deploy the contract using Clarinet
2. Register tools using `register-tool`
3. Renters can view available tools and initiate rentals
4. Return process automatically handles deposit returns

## 💡 Example Usage

```clarity
;; Register a new tool
(contract-call? .tool-rental register-tool u1 "Tractor" u100 u1000)

;; Rent a tool
(contract-call? .tool-rental rent-tool u1 u7) ;; Rent for 7 days

;; Return a tool
(contract-call? .tool-rental return-tool u1)
```

## 🔒 Security

- Automated deposit management
- Time-locked agreements
- Penalty enforcement for late returns
```
