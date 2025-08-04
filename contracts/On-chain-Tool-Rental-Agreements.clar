(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-TOOL-NOT-FOUND (err u101))
(define-constant ERR-TOOL-NOT-AVAILABLE (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-NO-ACTIVE-RENTAL (err u104))
(define-constant ERR-RENTAL-NOT-EXPIRED (err u105))
(define-constant ERR-INVALID-RATING (err u106))
(define-constant ERR-INSUFFICIENT-TOOLS (err u107))
(define-constant ERR-TOOL-OWNERSHIP-MISMATCH (err u108))
(define-constant ERR-PACKAGE-NOT-FOUND (err u109))

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
        
        (update-rental-history tool-id duration total-cost (get deposit-amount tool))
        
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
        
        (complete-rental-history tool-id tx-sender)
        
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

(define-map tool-ratings
    { tool-id: uint }
    {
        total-rating: uint,
        rating-count: uint,
        average-rating: uint
    }
)

(define-map tool-reviews
    { tool-id: uint, review-id: uint }
    {
        renter: principal,
        rating: uint,
        review-text: (string-ascii 200),
        timestamp: uint
    }
)

(define-map tool-review-counters
    { tool-id: uint }
    { count: uint }
)

(define-public (rate-tool (tool-id uint) (rating uint) (review-text (string-ascii 200)))
    (let
        ((rental (unwrap! (map-get? rentals { tool-id: tool-id }) ERR-NO-ACTIVE-RENTAL))
         (current-ratings (default-to { total-rating: u0, rating-count: u0, average-rating: u0 } 
                          (map-get? tool-ratings { tool-id: tool-id })))
         (review-counter (default-to { count: u0 } 
                         (map-get? tool-review-counters { tool-id: tool-id })))
         (new-review-id (+ (get count review-counter) u1))
         (new-total-rating (+ (get total-rating current-ratings) rating))
         (new-rating-count (+ (get rating-count current-ratings) u1))
         (new-average-rating (/ new-total-rating new-rating-count)))
        
        (asserts! (is-eq (get renter rental) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get returned rental) ERR-NO-ACTIVE-RENTAL)
        (asserts! (and (>= rating u1) (<= rating u5)) (err u106))
        
        (map-set tool-ratings
            { tool-id: tool-id }
            {
                total-rating: new-total-rating,
                rating-count: new-rating-count,
                average-rating: new-average-rating
            })
            
        (map-set tool-reviews
            { tool-id: tool-id, review-id: new-review-id }
            {
                renter: tx-sender,
                rating: rating,
                review-text: review-text,
                timestamp: stacks-block-height
            })
            
        (ok (map-set tool-review-counters
            { tool-id: tool-id }
            { count: new-review-id }))
    )
)

(define-read-only (get-tool-rating (tool-id uint))
    (ok (map-get? tool-ratings { tool-id: tool-id }))
)

(define-read-only (get-tool-review (tool-id uint) (review-id uint))
    (ok (map-get? tool-reviews { tool-id: tool-id, review-id: review-id }))
)

(define-map rental-packages
    { package-id: uint }
    {
        name: (string-ascii 50),
        tool-ids: (list 10 uint),
        package-daily-rate: uint,
        package-deposit: uint,
        available: bool,
        owner: principal
    }
)

(define-map package-rentals
    { package-id: uint }
    {
        renter: principal,
        start-time: uint,
        duration: uint,
        deposit-paid: uint,
        returned: bool
    }
)

(define-data-var next-package-id uint u1)

(define-public (create-rental-package (name (string-ascii 50)) (tool-ids (list 10 uint)) (package-daily-rate uint) (package-deposit uint))
    (let
        ((package-id (var-get next-package-id))
         (caller tx-sender))
        
        (asserts! (is-eq caller (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> (len tool-ids) u1) (err u107))
        (asserts! (fold check-tool-ownership tool-ids true) (err u108))
        
        (var-set next-package-id (+ package-id u1))
        
        (ok (map-set rental-packages
            { package-id: package-id }
            {
                name: name,
                tool-ids: tool-ids,
                package-daily-rate: package-daily-rate,
                package-deposit: package-deposit,
                available: true,
                owner: caller
            }))
    )
)

(define-public (rent-package (package-id uint) (duration uint))
    (let
        ((package (unwrap! (map-get? rental-packages { package-id: package-id }) (err u109)))
         (total-cost (+ (* (get package-daily-rate package) duration) (get package-deposit package))))
        
        (asserts! (get available package) ERR-TOOL-NOT-AVAILABLE)
        (asserts! (>= (stx-get-balance tx-sender) total-cost) ERR-INSUFFICIENT-FUNDS)
        (asserts! (fold check-tools-available (get tool-ids package) true) ERR-TOOL-NOT-AVAILABLE)
        
        (try! (stx-transfer? total-cost tx-sender (get owner package)))
        
        (fold set-tools-unavailable (get tool-ids package) true)
        
        (map-set rental-packages
            { package-id: package-id }
            (merge package { available: false }))
            
        (ok (map-set package-rentals
            { package-id: package-id }
            {
                renter: tx-sender,
                start-time: stacks-block-height,
                duration: duration,
                deposit-paid: (get package-deposit package),
                returned: false
            }))
    )
)

(define-public (return-package (package-id uint))
    (let
        ((rental (unwrap! (map-get? package-rentals { package-id: package-id }) ERR-NO-ACTIVE-RENTAL))
         (package (unwrap! (map-get? rental-packages { package-id: package-id }) (err u109))))
        
        (asserts! (is-eq (get renter rental) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (not (get returned rental)) ERR-TOOL-NOT-AVAILABLE)
        
        (fold set-tools-available (get tool-ids package) true)
        
        (map-set rental-packages
            { package-id: package-id }
            (merge package { available: true }))
            
        (map-set package-rentals
            { package-id: package-id }
            (merge rental { returned: true }))
            
        (if (> stacks-block-height (+ (get start-time rental) (get duration rental)))
            (stx-transfer? 
                (/ (get deposit-paid rental) u2)
                (get owner package)
                tx-sender)
            (stx-transfer? 
                (get deposit-paid rental)
                (get owner package)
                tx-sender)
        )
    )
)

(define-private (check-tool-ownership (tool-id uint) (acc bool))
    (match (map-get? tools { tool-id: tool-id })
        tool (and acc (is-eq (get owner tool) (var-get contract-owner)))
        false
    )
)

(define-private (check-tools-available (tool-id uint) (acc bool))
    (match (map-get? tools { tool-id: tool-id })
        tool (and acc (get available tool))
        false
    )
)

(define-private (set-tools-unavailable (tool-id uint) (acc bool))
    (match (map-get? tools { tool-id: tool-id })
        tool (begin
            (map-set tools { tool-id: tool-id } (merge tool { available: false }))
            acc)
        acc
    )
)

(define-private (set-tools-available (tool-id uint) (acc bool))
    (match (map-get? tools { tool-id: tool-id })
        tool (begin
            (map-set tools { tool-id: tool-id } (merge tool { available: true }))
            acc)
        acc
    )
)

(define-read-only (get-package-info (package-id uint))
    (ok (map-get? rental-packages { package-id: package-id }))
)

(define-read-only (get-package-rental-info (package-id uint))
    (ok (map-get? package-rentals { package-id: package-id }))
)

(define-map rental-history
    { renter: principal, rental-id: uint }
    {
        tool-id: uint,
        start-time: uint,
        duration: uint,
        total-cost: uint,
        deposit-paid: uint,
        completed: bool
    }
)

(define-map renter-stats
    { renter: principal }
    {
        total-rentals: uint,
        total-spent: uint,
        active-rentals: uint
    }
)

(define-map tool-stats
    { tool-id: uint }
    {
        total-rentals: uint,
        total-revenue: uint,
        total-rental-days: uint
    }
)

(define-map owner-stats
    { owner: principal }
    {
        total-revenue: uint,
        total-tools-rented: uint,
        active-tool-rentals: uint
    }
)

(define-data-var next-rental-id uint u1)

(define-private (update-rental-history (tool-id uint) (duration uint) (total-cost uint) (deposit-paid uint))
    (let
        ((rental-id (var-get next-rental-id))
         (current-renter-stats (default-to { total-rentals: u0, total-spent: u0, active-rentals: u0 }
                               (map-get? renter-stats { renter: tx-sender })))
         (current-tool-stats (default-to { total-rentals: u0, total-revenue: u0, total-rental-days: u0 }
                             (map-get? tool-stats { tool-id: tool-id })))
         (tool-owner (get owner (unwrap-panic (map-get? tools { tool-id: tool-id }))))
         (current-owner-stats (default-to { total-revenue: u0, total-tools-rented: u0, active-tool-rentals: u0 }
                              (map-get? owner-stats { owner: tool-owner }))))
        
        (var-set next-rental-id (+ rental-id u1))
        
        (map-set rental-history
            { renter: tx-sender, rental-id: rental-id }
            {
                tool-id: tool-id,
                start-time: stacks-block-height,
                duration: duration,
                total-cost: total-cost,
                deposit-paid: deposit-paid,
                completed: false
            })
            
        (map-set renter-stats
            { renter: tx-sender }
            {
                total-rentals: (+ (get total-rentals current-renter-stats) u1),
                total-spent: (+ (get total-spent current-renter-stats) total-cost),
                active-rentals: (+ (get active-rentals current-renter-stats) u1)
            })
            
        (map-set tool-stats
            { tool-id: tool-id }
            {
                total-rentals: (+ (get total-rentals current-tool-stats) u1),
                total-revenue: (+ (get total-revenue current-tool-stats) total-cost),
                total-rental-days: (+ (get total-rental-days current-tool-stats) duration)
            })
            
        (map-set owner-stats
            { owner: tool-owner }
            {
                total-revenue: (+ (get total-revenue current-owner-stats) total-cost),
                total-tools-rented: (+ (get total-tools-rented current-owner-stats) u1),
                active-tool-rentals: (+ (get active-tool-rentals current-owner-stats) u1)
            })
    )
)

(define-private (complete-rental-history (tool-id uint) (renter principal))
    (let
        ((rental-data (unwrap-panic (map-get? rentals { tool-id: tool-id })))
         (current-renter-stats (unwrap-panic (map-get? renter-stats { renter: renter })))
         (tool-owner (get owner (unwrap-panic (map-get? tools { tool-id: tool-id }))))
         (current-owner-stats (unwrap-panic (map-get? owner-stats { owner: tool-owner }))))
        
        (map-set renter-stats
            { renter: renter }
            (merge current-renter-stats 
                   { active-rentals: (- (get active-rentals current-renter-stats) u1) }))
                   
        (map-set owner-stats
            { owner: tool-owner }
            (merge current-owner-stats 
                   { active-tool-rentals: (- (get active-tool-rentals current-owner-stats) u1) }))
    )
)

(define-read-only (get-renter-history (renter principal) (start-id uint) (limit uint))
    (ok (filter-rental-history renter start-id limit))
)

(define-read-only (get-renter-stats (renter principal))
    (ok (map-get? renter-stats { renter: renter }))
)

(define-read-only (get-tool-stats (tool-id uint))
    (ok (map-get? tool-stats { tool-id: tool-id }))
)

(define-read-only (get-owner-stats (owner principal))
    (ok (map-get? owner-stats { owner: owner }))
)

(define-read-only (get-rental-by-id (renter principal) (rental-id uint))
    (ok (map-get? rental-history { renter: renter, rental-id: rental-id }))
)

(define-private (filter-rental-history (renter principal) (start-id uint) (limit uint))
    (let
        ((end-id (+ start-id limit)))
        (map get-rental-record (list start-id (+ start-id u1) (+ start-id u2) (+ start-id u3) (+ start-id u4)))
    )
)

(define-private (get-rental-record (rental-id uint))
    (map-get? rental-history { renter: tx-sender, rental-id: rental-id })
)