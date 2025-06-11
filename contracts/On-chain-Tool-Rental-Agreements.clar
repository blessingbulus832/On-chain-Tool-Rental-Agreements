(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-TOOL-NOT-FOUND (err u101))
(define-constant ERR-TOOL-NOT-AVAILABLE (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-NO-ACTIVE-RENTAL (err u104))
(define-constant ERR-RENTAL-NOT-EXPIRED (err u105))

(define-data-var contract-owner principal tx-sender)

(define-map tools 
    { tool-id: uint }
    {
        name: (string-ascii 50),
        daily-rate: uint,
        deposit-amount: uint,
        available: bool,
        owner: principal
    }
)

(define-map rentals
    { tool-id: uint }
    {
        renter: principal,
        start-time: uint,
        duration: uint,
        deposit-paid: uint,
        returned: bool
    }
)

(define-public (register-tool (tool-id uint) (name (string-ascii 50)) (daily-rate uint) (deposit-amount uint))
    (let
        ((caller tx-sender))
        (asserts! (is-eq caller (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set tools
            { tool-id: tool-id }
            {
                name: name,
                daily-rate: daily-rate,
                deposit-amount: deposit-amount,
                available: true,
                owner: caller
            }
        ))
    )
)

(define-public (rent-tool (tool-id uint) (duration uint))
    (let
        ((tool (unwrap! (map-get? tools { tool-id: tool-id }) ERR-TOOL-NOT-FOUND))
         (total-cost (+ (* (get daily-rate tool) duration) (get deposit-amount tool))))
        
        (asserts! (get available tool) ERR-TOOL-NOT-AVAILABLE)
        (asserts! (>= (stx-get-balance tx-sender) total-cost) ERR-INSUFFICIENT-FUNDS)
        
        (try! (stx-transfer? total-cost tx-sender (get owner tool)))
        
        (map-set tools
            { tool-id: tool-id }
            (merge tool { available: false }))
            
        (ok (map-set rentals
            { tool-id: tool-id }
            {
                renter: tx-sender,
                start-time: stacks-block-height,
                duration: duration,
                deposit-paid: (get deposit-amount tool),
                returned: false
            }))
    )
)

(define-public (return-tool (tool-id uint))
    (let
        ((rental (unwrap! (map-get? rentals { tool-id: tool-id }) ERR-NO-ACTIVE-RENTAL))
         (tool (unwrap! (map-get? tools { tool-id: tool-id }) ERR-TOOL-NOT-FOUND)))
        
        (asserts! (is-eq (get renter rental) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (not (get returned rental)) ERR-TOOL-NOT-AVAILABLE)
        
        (map-set tools
            { tool-id: tool-id }
            (merge tool { available: true }))
            
        (map-set rentals
            { tool-id: tool-id }
            (merge rental { returned: true }))
            
        (if (> stacks-block-height (+ (get start-time rental) (get duration rental)))
            (stx-transfer? 
                (/ (get deposit-paid rental) u2)
                (get owner tool)
                tx-sender)
            (stx-transfer? 
                (get deposit-paid rental)
                (get owner tool)
                tx-sender)
        )
    )
)

(define-read-only (get-tool-info (tool-id uint))
    (ok (map-get? tools { tool-id: tool-id }))
)

(define-read-only (get-rental-info (tool-id uint))
    (ok (map-get? rentals { tool-id: tool-id }))
)
