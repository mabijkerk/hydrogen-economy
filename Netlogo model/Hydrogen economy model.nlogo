;Copyright (c) 2020 Mathijs Bijkerk

;Permission is hereby granted, free of charge, to any person obtaining a copy
;of this software and associated documentation files (the "Software"), to deal
;in the Software without restriction, including without limitation the rights
;to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;copies of the Software, and to permit persons to whom the Software is
;furnished to do so, subject to the following conditions:

;The above copyright notice and this permission notice shall be included in all
;copies or substantial portions of the Software.

;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;SOFTWARE.

__includes [ "globals.nls"                          ;; these files contain globals, breeds & setup procedures
             "breeds.nls"
             "setup.nls"
           ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go Procedure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  if ticks >= 30 [ stop ]                           ;; tick 0 represent 2020 and tick 30 represents 2050, so after tick 30 the model stops

  ;; preparation phase
  sectors-update-demand                             ;; external development is updated, which includes all externals except for the prices, which are assumed to 'arise' during the year and are therefore updated later
  update-technologies
  update-policies
  production-assets-offer-contracts                 ;; any production assets that have finished production at the end of the previous year will first offer contracts to the demand sectors
  sectors-select-contracts

  ;; planning phase
  companies-set-budgets
  companies-set-expected-demand
  companies-plan-knowledge-investments
  companies-plan-asset-investments                  ;; initiation phase is part of companies-plan-asset-investments

  ;; execution phase
  update-prices
  companies-do-knowledge-investments
  companies-do-asset-investments

  ;; operation phase
  companies-operate-assets

  renew-plots                                       ;; procedure to update custom plots

  tick

  set TIME ticks + 2020

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preparation phase ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to sectors-update-demand

  ask sectors                                                                                                                                   ;; sectors update their demand based on what year it is and the demand list generated in setup
  [ set demand item ticks demand-list
    set sector-contracted-production ( sum [ contract-quantity ] of (my-contracts with [contract-status = "accepted"]) * PJ-per-MWh )           ;; demand is given in PJ, production is given in MWh
    set sector-demand-shortage demand - sector-contracted-production                                                                            ;; sectors update their demand shortage based on contracted production

    ;show demand
    ;show sector-contracted-production
    ;show sector-demand-shortage

  ]

end


to update-technologies

  let i 0                                                                                                                                       ;; each technology checks whether there is external technological development
  while [ i < length technologies ]
  [ let current-technology                     item i technologies
    let current-initial-CAPEX                  item technology-CAPEX current-technology                                                         ;; all the elements which have external technological development are selected
    let current-efficiency                     item technology-efficiency current-technology
    let current-TRL                            item technology-TRL current-technology

    if current-TRL = 9                                                                                                                          ;; only technologies with TRL < 9 have external development
    [ set efficiency-list-2030                 replace-item i efficiency-list-2030   0
      set efficiency-list-2050                 replace-item i efficiency-list-2050   0
      set yearly-TRL-change                    replace-item i yearly-TRL-change      0
    ]

    if ticks > 0                                                                                                                                ;; learning-by-searching and efficiency are not updated in 2020 (initial values from setup are used)
    [ ifelse ticks <= 10                                                                                                                        ;; learning-by-searching and efficiency have different development parameters for 2020 to 2030 and 2030 to 2050
      [ set current-initial-CAPEX            max (list 0 (precision (current-initial-CAPEX - item i learning-by-searching-list-2030) 4))        ;; external technological development through learning-by-searching leads to a yearly linear increase in CAPEX
        set current-efficiency               min (list 1 (precision (current-efficiency    + item i efficiency-list-2030           ) 4))        ;; external technological development leads to a yearly linear increase in efficiency
      ]
      [ set current-initial-CAPEX            max (list 0 (precision (current-initial-CAPEX - item i learning-by-searching-list-2050) 4))        ;; using max 0 to prevent CAPEX from becoming negative through external development
        set current-efficiency               min (list 1 (precision (current-efficiency    + item i efficiency-list-2050           ) 4))        ;; using min 1 to prevent efficiency from becoming larger than 1 through external development
      ]
    ]
    let old-TRL                              current-TRL                                                                                        ;; this is used to check if the TRL has been updated, in which case the technology risk also has to be updated
    if current-TRL != 9
    [ set current-TRL                        (floor (item i initial-TRL-list + ticks * item i yearly-TRL-change))                               ;; TRL is increased every year, but only once the next TRL level is reached is the TRL in the technologies list updated
    ]
    let current-technology-risk              (precision (technology-risk-b - technology-risk-a  * current-TRL) 2)                               ;; technology risk is inversely dependent on TRL

    set current-technology                   replace-item technology-CAPEX current-technology current-initial-CAPEX
    set current-technology                   replace-item technology-efficiency current-technology current-efficiency
    set current-technology                   replace-item technology-TRL current-technology current-TRL
    set current-technology                   replace-item technology-risk current-technology current-technology-risk
    set technologies                         replace-item i technologies current-technology

    ask companies
    [ let current-company-technology         item i company-technologies                                                                        ;; companies must update their technologies according to the external development of the general technologies

      set current-company-technology         replace-item technology-efficiency current-company-technology current-efficiency

      set current-initial-CAPEX              item technology-CAPEX                    current-technology                                        ;; update technology risk if through external development CAPEX has been decreased
      let current-initial-capacity           item technology-capacity                 current-technology
      let current-cumulative-capacity        item technology-cumulative-capacity      current-company-technology
      let current-learning-by-doing          item technology-learning-by-doing        current-company-technology
      ifelse current-cumulative-capacity >= current-initial-capacity
      [ let current-CAPEX                    current-initial-CAPEX * (( current-cumulative-capacity / current-initial-capacity ) ^ (- current-learning-by-doing))
        set current-company-technology       replace-item technology-CAPEX            current-company-technology current-CAPEX
      ]
      [ let current-CAPEX                    current-initial-CAPEX
        set current-company-technology       replace-item technology-CAPEX            current-company-technology current-CAPEX
      ]

      if current-TRL > old-TRL                                                                                                                  ;; update technology risk if through external development TRL has been increased
      [ let current-initial-technology-risk  item i initial-technology-risk-list
        set current-initial-technology-risk  precision (current-initial-technology-risk - technology-risk-a ) 4
        set initial-technology-risk-list     replace-item i initial-technology-risk-list current-initial-technology-risk

        let cumulative-investment            item technology-cumulative-knowledge current-company-technology
        ifelse cumulative-investment >= knowledge-costs-factor
        [ set current-technology-risk        precision (current-initial-technology-risk * ( (cumulative-investment / knowledge-costs-factor) ^ (- knowledge-factor) )) 4
        ]
        [ set current-technology-risk        current-initial-technology-risk                                                                    ;; if not enough has been invested yet, then the technology risk has not yet been updated by a company, meaning that it is still equal to the initial technology risk
        ]

        set current-company-technology       replace-item technology-TRL                    current-company-technology current-TRL
        set current-company-technology       replace-item technology-risk                   current-company-technology current-technology-risk
      ]

      set company-technologies               replace-item i company-technologies current-company-technology
    ]

    set i i + 1
  ]

  ;show technologies
  ;show item technology-ALK technologies
  ;ask company 6 [show item technology-SOEC company-technologies]

end


to update-policies

  if (ticks = policy-public-infrastructure-first-year) and (public-infrastructure-available? = true)
  [ set policy-public-infrastructure-active? true
  ]

  if (ticks = policy-blending-first-year) and (blending-available? = true)
  [ set policy-blending-active? true
  ]

  if (ticks = policy-carbon-tax-first-year) and (carbon-tax-available? = true)
  [ set policy-carbon-tax-active? true
    set policy-carbon-tax         30
    set carbon-price max (list ETS-price policy-carbon-tax)
  ]

  if (ticks > policy-carbon-tax-first-year) and (policy-carbon-tax-active? = true)
  [ ifelse ticks <= 10
    [ set policy-carbon-tax policy-carbon-tax + policy-carbon-tax-2030
    ]
    [ set policy-carbon-tax policy-carbon-tax + policy-carbon-tax-2050
    ]
    set carbon-price max (list ETS-price policy-carbon-tax)
  ]

  ;show policy-blending-capacity
  ;show policy-blending-capacity-available
  ;show policy-carbon-tax
  ;show policy-carbon-tax-active?

end

to production-assets-offer-contracts                                                                                                            ;; A44 sector choice is set at the planning of production-assets and are fixed during lifetime

  ask production-assets with [ production-asset-status = "idle" ]                                                                               ;; A9, if the production-asset is idle, the hydrogen price for my-contracts is calculated in order to propose the contracts
  [ determine-contract-price

    ask my-contracts
    [ set contract-status          "proposed"
      set contract-price           [production-asset-hydrogen-price] of end2
      set contract-duration        ( min ( list ( [production-asset-lifetime - production-asset-age] of end2 ) standard-contract-duration ))    ;; A1 standard contract duration is set to 10 years, however shorter if remaining lifetime is shorter
      set contract-first-year      ticks
      set contract-last-year       contract-first-year + contract-duration -  1
      set color                    yellow
    ]
  ]

end


to determine-contract-price

  ask my-contracts                                                                                                                              ;; A2, grid purchasing of energy source is assumed and fixed for contract duration
  [ ifelse ([energy-source] of end2) = "natural-gas"
    [ set contract-energy-price natural-gas-price
    ]
    [ set contract-energy-price electricity-price
    ]
  ]

  let force-sustainability? false
  if SOE1-active? = true
  [ if (SOE1-sustainable? = true) and (member? SOE1 my-participants)
    [ set force-sustainability? true ]
  ]

  ifelse force-sustainability? = true
  [ set production-asset-sustainable? true
  ]
  [ ifelse energy-source = "natural-gas"
    [ ifelse (CCS-price * production-asset-emission-factor-0 + carbon-price * (1 - production-asset-emission-factor-0)) <= carbon-price         ;; A31, A32 all production-assets using natural gas can enable CCS, both CCS-price and carbon-price are given in €/ton CO2
      [ set production-asset-sustainable? true ]
      [ set production-asset-sustainable? false ]
    ]
    [ ifelse GO-price <= carbon-price * production-asset-emission-factor                                                                        ;; all production-assets using electricity can buy GOs, GO-price is in €/MWh (premium on top of normal electricity price), carbon-price is therefore converted to €/MWh as well
      [ set production-asset-sustainable? true ]
      [ set production-asset-sustainable? false ]
    ]
  ]

  determine-variable-OPEX                                                                                                                       ;; A34 determining hydrogen price is done based on current hydrogen prices

  let OPEX-per-MWh (energy-source-OPEX + sustainability-OPEX + transport-OPEX + production-asset-fixed-OPEX / production-asset-capacity / operating-hours)            ;; in €/MWh

  let remaining-lifetime production-asset-lifetime - production-asset-age
  let years-to-recover-CAPEX min (list standard-contract-duration remaining-lifetime)                                                           ;; A3, A4 if some part of CAPEX still has to be recovered then this is assumed to be done within the next contract, or the remaining lifetime
  let CAPEX-to-recover max (list (production-asset-total-CAPEX * production-asset-ROI - production-asset-cumulative-profit) 0)                  ;; A3, A4 it is calculated how much CAPEX still has to be recovered, once enough profit has been made no CAPEX needs to be recovered and only ROI on OPEX is expected
  let CAPEX-per-MWh (CAPEX-to-recover) / production-asset-capacity / (years-to-recover-CAPEX) / operating-hours                                 ;; in €/MWh

  set production-asset-hydrogen-price CAPEX-per-MWh + OPEX-per-MWh * production-asset-ROI                                                       ;; A3, A4

  ;show CAPEX-to-recover
  ;show CAPEX-per-MWh
  ;show production-asset-hydrogen-price

end


to determine-variable-OPEX

  set transport-OPEX sum [contract-transport-OPEX] of my-contracts

  set energy-source-OPEX (mean [contract-energy-price] of my-contracts) / production-asset-efficiency                                           ;; in €/MWh

  ifelse energy-source = "natural-gas"                                                                                                          ;; in €/MWh
  [ ifelse production-asset-sustainable? = true
    [ set sustainability-OPEX (CCS-price * production-asset-emission-factor-0 + carbon-price * (1 - production-asset-emission-factor-0)) * production-asset-emission-factor       ;; not divided by efficiency because emission factor is given per MWh H2 produced
    ]
    [ set sustainability-OPEX carbon-price * production-asset-emission-factor                                                                   ;; not divided by efficiency because emission factor is given per MWh H2 produced
    ]
  ]
  [
    ifelse production-asset-sustainable? = true
    [ set sustainability-OPEX GO-price  / production-asset-efficiency                                                                           ;; divided by efficiency because emission factor is determined using energy input
    ]
    [ set sustainability-OPEX carbon-price * production-asset-emission-factor / production-asset-efficiency
    ]
  ]

end


to sectors-select-contracts

  ask sectors
  [ let new-production-contracts my-contracts with [contract-status = "proposed"]                                                               ;; sectors select proposed contracts based on their demand shortage

    ;show my-contracts
    ;show new-production-contracts
    ;ask new-production-contracts [show contract-price]
    ;ask new-production-contracts [show contract-quantity * PJ-per-MWh]

    while [(sector-demand-shortage > 0) and (any? new-production-contracts)]                                                                    ;; A37 as long as there is demand shortage sectors select the cheapest contracts, the marginal contract is allowed to have some overcapacity
    [ ask min-one-of new-production-contracts [contract-price]
      [ set contract-status            "accepted"
        set contract-first-year        ticks

        ifelse [production-asset-sustainable?] of end2 = true
        [ ifelse [energy-source] of end2 = "natural-gas"
          [ set color blue
          ]
          [ set color green
          ]
        ]
        [ set color grey
        ]

        ask end2
        [ set years-idle 0
          set production-asset-status "operating"                                                                                               ;; A6, as soon as one contract of a production-asset is accepted, the asset is considered to be operating
        ]

      ]
      set sector-contracted-production ( sum [ contract-quantity ] of (my-contracts with [contract-status = "accepted"]) * PJ-per-MWh )         ;; contracted production is renewed, demand is given in PJ
      set sector-demand-shortage demand - sector-contracted-production                                                                          ;; demand shortage is renewed
      set new-production-contracts my-contracts with [contract-status = "proposed"]                                                             ;; new production is renewed

      ;show sector-demand-shortage

    ]
  ]

  ask production-assets with [ all? my-contracts [contract-status = "proposed"]]                                                                ;; A6, A9, after all sectors have, to the extent that new production has been offered to them, satisfied their demand shortage, any production assets with no contracts accepted are set to idle
  [ ask my-contracts
    [ set contract-status "idle"
      set color red
    ]
  ]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Planning phase ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to companies-set-budgets

  ask companies
  [ let expected-profit                sum ([share-profit] of my-shares)                                                                        ;; A38 no memory or prediction, just last years profit (meaning last years prices and last years production assets)
    set total-budget                   total-budget                                                                                             ;; A39 the total-budget has no external growth
    set investment-budget              max (list 0 (total-budget + expected-profit))                                                            ;; A64 a company can have a negative total-budget, in which case no further investments are made until the total-budget is positive again
    set asset-investment-budget        item 0 investment-preferences * investment-budget                                                        ;; A40 there is a pre-set distribution of investment budget which does not change

    ifelse (SOE1-active? = true) and (self = SOE1)                                                                                              ;; total budget of SOE1 is arbitrarily large, therefore it cannot base its knowledge budget on the total budget
    [ set individual-knowledge-budget    item 1 investment-preferences * knowledge-budget-SOE1
      set shared-knowledge-budget        item 2 investment-preferences * knowledge-budget-SOE1
      set publish-knowledge-budget       item 3 investment-preferences * knowledge-budget-SOE1
    ]
    [ set individual-knowledge-budget    item 1 investment-preferences * investment-budget
      set shared-knowledge-budget        item 2 investment-preferences * investment-budget
      set publish-knowledge-budget       item 3 investment-preferences * investment-budget
    ]

    set asset-investment-budget-reserved 0                                                                                                      ;; due to the difference between expected and actual investment costs and due to agent order in initiating or participating in assets companies need to track reserved investment budget

    ;show expected-profit
    ;show total-budget
  ]

end


to companies-set-expected-demand

  ask companies
  [ set expected-sector-shortages []                                                                                                            ;; demand is given in PJ

    (foreach sector-list market-risk-list                                                                                                       ;; for each sector the expected demand shortage is calculated using the corresponding market risk for that sector
     [ [ a b ] ->
      let expected-demand [item (ticks + 1) demand-list] of a                                                                                   ;; A12 companies look 1 tick into the future because construction time of all assets is set at 1 year
      set expected-demand random-normal expected-demand (expected-demand * b)                                                                   ;; A10 market risk is modelled as the st. dev. in the perception of the demand of a sector
      let currently-contracted-production [sector-contracted-production] of a
      set expected-sector-shortages lput (expected-demand - currently-contracted-production) expected-sector-shortages                          ;; A13 companies look at currently contracted production and expected demand to determine expected demand shortage
     ]
    )

    let max-expected-shortage max expected-sector-shortages                                                                                     ;; A62 preferred sector for knowledge investments is the sector with the largest expected demand shortage
    let current-position position max-expected-shortage expected-sector-shortages
    set sector-preference item current-position sector-list

    ;show expected-sector-shortages
    ;show sector-preference
  ]

end


to companies-plan-knowledge-investments

  if (SOE1-active? = true) and (SOE1-asset-investment = "no-asset-investment")                                                                  ;; if SOE1 does not invest in assets it updates technology preference for knowledge investments by choosing the dominant technology
  [ let installed-technologies [production-asset-type] of production-assets
    if not empty? installed-technologies
    [ let dominant-technology one-of modes installed-technologies
      ask SOE1
      [ set technology-preference dominant-technology
      ]
    ]
  ]

  ask companies
  [ if individual-knowledge-budget > 0
    [ plan-individual-knowledge
    ]
    if shared-knowledge-budget > 0
    [ plan-shared-knowledge
    ]
    if publish-knowledge-budget > 0
    [ plan-publish-knowledge
    ]
  ]

  ;ask companies [show company-technologies]
  ;ask companies [show sector-cumulative-knowledge]

end


to plan-individual-knowledge

  let current-position                   position sector-preference sector-list                                                                 ;; cumulative market knowledge investment is updated
  let current-sector-knowledge           item current-position sector-cumulative-knowledge
  set current-sector-knowledge           current-sector-knowledge + individual-knowledge-budget
  set sector-cumulative-knowledge        replace-item current-position sector-cumulative-knowledge current-sector-knowledge

  let i 0                                                                                                                                       ;; cumulative technology knowledge investment is updated
  while [ i < length technologies ]
  [ let current-technology               item i company-technologies
    if (item technology-type current-technology) = technology-preference
    [ let current-technology-knowledge   item technology-cumulative-knowledge current-technology
      set current-technology-knowledge   current-technology-knowledge + individual-knowledge-budget
      set current-technology             replace-item technology-cumulative-knowledge current-technology current-technology-knowledge
      set company-technologies           replace-item i company-technologies current-technology
    ]
    set i i + 1
  ]

  set total-budget total-budget - individual-knowledge-budget
  set cumulative-knowledge-investments cumulative-knowledge-investments + individual-knowledge-budget

  ;show individual-knowledge-budget
  ;show total-budget

end


to plan-shared-knowledge

  let shared-companies companies with [(technology-preference = ([technology-preference] of myself)) and (sector-preference = ([sector-preference] of myself))]
  if count shared-companies > 1                                                                                                                 ;; only if there are companies with the same sector-preference and technology-preference can a shared knowledge project be invested in
  [ let total-shared-investment sum [shared-knowledge-budget] of shared-companies                                                               ;; added investment for each participant is the total shared budget of all participants

    let current-position                   position sector-preference sector-list                                                               ;; cumulative market knowledge investment is updated
    let current-sector-knowledge           item current-position sector-cumulative-knowledge
    set current-sector-knowledge           current-sector-knowledge + total-shared-investment
    set sector-cumulative-knowledge        replace-item current-position sector-cumulative-knowledge current-sector-knowledge

    let i 0                                                                                                                                     ;; cumulative technology knowledge investment is updated
    while [ i < length technologies ]
    [ let current-technology               item i company-technologies
      if (item technology-type current-technology) = technology-preference
      [ let current-technology-knowledge   item technology-cumulative-knowledge current-technology
        set current-technology-knowledge   current-technology-knowledge + total-shared-investment
        set current-technology             replace-item technology-cumulative-knowledge current-technology current-technology-knowledge
        set company-technologies           replace-item i company-technologies current-technology
      ]
      set i i + 1
    ]

    set total-budget total-budget - shared-knowledge-budget
    set cumulative-knowledge-investments cumulative-knowledge-investments + shared-knowledge-budget
  ]

  ;show shared-knowledge-budget
  ;ask shared-companies [show self]
  ;show total-budget

end


to plan-publish-knowledge

  let current-sector-preference            sector-preference                                                                                    ;; company that publishes knowledge sets the market and the technology
  let current-technology-preference        technology-preference
  let current-publish-budget               publish-knowledge-budget

  ask companies                                                                                                                                 ;; all companies benefit from a publish knowledge project, including the agent itself
  [ let current-position                   position current-sector-preference sector-list                                                       ;; cumulative market knowledge investment is updated
    let current-sector-knowledge           item current-position sector-cumulative-knowledge
    set current-sector-knowledge           current-sector-knowledge + current-publish-budget
    set sector-cumulative-knowledge        replace-item current-position sector-cumulative-knowledge current-sector-knowledge

    let i 0                                                                                                                                     ;; cumulative technology knowledge investment is updated
    while [ i < length technologies ]
    [ let current-technology               item i company-technologies
      if (item technology-type current-technology) = current-technology-preference
      [ let current-technology-knowledge   item technology-cumulative-knowledge current-technology
        set current-technology-knowledge   current-technology-knowledge + current-publish-budget
        set current-technology             replace-item technology-cumulative-knowledge current-technology current-technology-knowledge
        set company-technologies           replace-item i company-technologies current-technology
      ]
      set i i + 1
    ]

    ;show current-publish-budget
  ]

  set total-budget total-budget - publish-knowledge-budget
  set cumulative-knowledge-investments cumulative-knowledge-investments + publish-knowledge-budget

  ;show publish-knowledge-budget
  ;show total-budget

end


to companies-plan-asset-investments

  let current-companies companies                                                                                                                  ;; check whether SOE1 does asset-investments or not
  if (SOE1-active? = true) and (SOE1-asset-investment != "initiation")                                                                             ;; only when SOE1 initiates production assets does it need to go through this procedure, in other cases it can be asked by other companies as a participant
  [ set current-companies current-companies with [self != SOE1]
  ]
  if SOE2-active? = true
  [ set current-companies current-companies with [self != SOE2]                                                                                    ;; SOE2 does not initiate assets
  ]

  ask current-companies
  [ if asset-investment-budget > 0
    [ loop                                                                                                                                         ;; A70 companies try to plan additional assets as long as the plan procedure is succesfull, when an asset is not succesfully planned the company stops furhter planning in this year
      [ let my-current-assets count my-production-assets
        hatch-production-assets 1                                                                                                                  ;; A42 production assets are planned one by one with a fixed capacity
        [ initiate
        ]
        if my-current-assets = count my-production-assets [ stop ]                                                                                 ;; A29 if no assets have been succesfully planned, then they have die in the initiate procedure, and no new assets are planned
      ]
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initiation phase ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to initiate

  create-share-from myself                                                                                                                       ;; set the company who initiated the production asset as the only participant so far
  [ set share-percentage 1
  ]
  set my-participants in-share-neighbors

  let my-potential-sectors sector-list                                                                                                           ;; all sectors are available
  let my-potential-sectors-shortages [expected-sector-shortages] of one-of my-participants                                                       ;; select the expected shortages of the only participant so far

  if empty? (filter [ i -> i > 0 ] my-potential-sectors-shortages)                                                                               ;; A43 if no sector has an expected demand shortage that is positive, the asset will not be planned
  [ set reporter-planning-demand-shortage lput 0 reporter-planning-demand-shortage
    die
  ]

  let max-sector-shortage max my-potential-sectors-shortages                                                                                     ;; A73 select the maximum in the list with expected shortages for all sectors
  let current-position (position max-sector-shortage my-potential-sectors-shortages)                                                             ;; position of the the maximum demand in the list with expected shortages for all sectors is retrieved
  set my-sectors (item current-position my-potential-sectors)                                                                                    ;; A15 the first sector added to my-sectors is the sector with the maximum expected shortage
  set production-asset-market-risk (item current-position ([market-risk-list] of one-of my-participants))

  if (SOE1-active? = true) and (SOE1-asset-investment = "mandatory-participation") and (self != SOE1)                                            ;; if SOE1 can initiate production assets it does not need to look for mandatory participants
  [ set-mandatory-participant current-position ]                                                                                                 ;; current-position is used as input for the procedure to allow mandatory participant SOE1 to select the market risk for the right sector

  let average-risk-appetite (mean ([risk-appetite] of my-participants))                                                                          ;; A20 if new participants are added then the average of their risk appetite is taken as a threshold value
  let i 0                                                                                                                                        ;; A71 to ensure that participants can select a techology the minimum technology risk must be taken into account when evaluating market risk and risk appetite
  let min-technology-risk []
  while [ i < length technologies ]
  [ let current-technology-risk min ([item technology-risk (item i company-technologies)] of my-participants)
    set min-technology-risk lput current-technology-risk min-technology-risk
    set i i + 1
  ]
  set min-technology-risk min min-technology-risk

  while [(production-asset-market-risk + min-technology-risk) > average-risk-appetite]                                                           ;; A71, A19 search for new market participants procedure is done based on market risk of first sector and on the minimum technology risk of participants
  [ if count my-participants >= max-participants                                                                                                 ;; A18 check if the maximum amount of participants has been reached
    [ set reporter-planning-risk lput 1 reporter-planning-risk
      die
    ]
    if self = SOE1                                                                                                                               ;; if SOE1 can initiate production assets it does not look for other participants
    [
      set reporter-planning-risk lput 1 reporter-planning-risk
      die
    ]
    search-participant current-position                                                                                                          ;; current-position is used as input for the procedure to allow new participants to select the market risk for the right sector
    set average-risk-appetite (mean ([risk-appetite] of my-participants))                                                                        ;; market risk has been updated with the search-participant procedure, average risk appetite and min technology risk are updated

    set i 0
    set min-technology-risk []
    while [ i < 5 ]
    [ let current-technology-risk min ([item technology-risk (item i company-technologies)] of my-participants)
      set min-technology-risk lput current-technology-risk min-technology-risk
      set i i + 1
    ]
    set min-technology-risk min min-technology-risk
  ]

  set-production-technology                                                                                                                      ;; choose the production-technology
  set-sectors my-potential-sectors my-potential-sectors-shortages                                                                                ;; check whether additional sectors should be selected, if the demand of one sector is too limited
  set-total-CAPEX my-expected-production-technology                                                                                              ;; total CAPEX in is € calculated here based on the expected costs, this includes transport CAPEX

  let enough-budget? false
  let my-enough-budget? []
  ask my-shares                                                                                                                                  ;; check whether the budgets of my-participants are enough to pay for the investment costs
  [ ifelse share-percentage * [production-asset-total-CAPEX] of end2 <= [asset-investment-budget - asset-investment-budget-reserved] of end1
    [ set my-enough-budget? lput true my-enough-budget?
    ]
    [ set my-enough-budget? lput false my-enough-budget?
    ]
  ]
  if not member? false my-enough-budget?
  [ set enough-budget? true
  ]

  ask my-participants                                                                                                                            ;; necessary to set my-production-assets here, because then if a participant is added the technology costs of that participant can be added to the production-asset costs
  [ set my-production-assets out-share-neighbors
  ]

  while [enough-budget? = false]                                                                                                                 ;; if the budgets of my-participants are not enough to pay for the investment costs search for new participants
  [ if count my-participants >= max-participants
    [ set reporter-planning-capital-intensity lput 2 reporter-planning-capital-intensity
      die
    ]
    if self = SOE1                                                                                                                               ;; if SOE1 can initiate production assets it does not look for other participants
    [
      set reporter-planning-capital-intensity lput 2 reporter-planning-capital-intensity
      die
    ]
    search-participant current-position                                                                                                          ;; market risk needs to be updated as well to set the ROI, which is why the same search participant function can be used
    set average-risk-appetite (mean ([risk-appetite] of my-participants))

    update-my-production-technologies                                                                                                            ;; if a new participant is added the specifications of the expected production technology and the actual production technology need to be updated, the production technology is not chosen again, which is why the set-production-technology procedure cannot be used again

    set-total-CAPEX my-expected-production-technology                                                                                            ;; using the updated my-production-technologies the total-CAPEX is recalculated as well

    ask my-participants
    [ set my-production-assets out-share-neighbors
    ]

    set my-enough-budget? []

    ask my-shares                                                                                                                                ;; check whether the budgets of my-participants are enough to pay for the investment costs
    [ ifelse share-percentage * [production-asset-total-CAPEX] of end2 <= [asset-investment-budget - asset-investment-budget-reserved] of end1
      [ set my-enough-budget? lput true my-enough-budget?
      ]
      [ set my-enough-budget? lput false my-enough-budget?
      ]
    ]
    if not member? false my-enough-budget?
    [ set enough-budget? true
    ]
  ]

  ask my-shares
  [ let current-share-percentage share-percentage
    let current-total-CAPEX [production-asset-total-CAPEX] of end2
    ask end1                                                                                                                                     ;; all participants update their reserved budget according to the share they have in the investment
    [ set asset-investment-budget-reserved asset-investment-budget-reserved + current-share-percentage * current-total-CAPEX
    ]
  ]

  ask my-contracts                                                                                                                               ;; ask participants to update their expected sector shortages
  [ set current-position position end1 sector-list
    let current-quantity contract-quantity * PJ-per-MWh
    ask end2
    [ ask my-participants
      [ let current-sector-shortage item current-position expected-sector-shortages
        set current-sector-shortage current-sector-shortage - current-quantity
        set expected-sector-shortages replace-item current-position expected-sector-shortages current-sector-shortage

        ;show current-position
        ;show current-sector-shortage
        ;show expected-sector-shortages
      ]
    ]
  ]

  ask my-participants                                                                                                                            ;; A63 preferred technology for knowledge investments is the last technology which has been succesfully planned
  [ set technology-preference [production-asset-type] of myself
  ]

  set production-asset-status "planned"

  set reporter-planning-success lput 4 reporter-planning-success

end


to set-mandatory-participant [current-position]                                                                                                  ;; A58 no effect of having more participants is assumed

  let potential-new-market-risk (item current-position ([market-risk-list] of SOE1))
  set production-asset-market-risk min (list potential-new-market-risk production-asset-market-risk)
  ask my-shares
  [ set share-percentage 1 - participation-percentage ]                                                                                          ;; SOE1 is the first participant and has a fixed participation percentage, the initiator of the asset therfore adjust its share accordingly
  create-share-from SOE1
  [ set share-percentage participation-percentage ]                                                                                              ;; SOE1 also sets its share with the fixed participation percentage
  set my-participants in-share-neighbors

end


to search-participant [current-position]                                                                                                         ;; A58 no effect of having more participants is assumed

  let potential-participants companies with [not member? self [my-participants] of myself]
  if (SOE1-active? = true) and (SOE1-asset-investment != "non-mandatory-participation")                                                          ;; only if SOE1 has non-mandatory participation can SOE1 be considered a potential participant
  [ set potential-participants potential-participants with [self != SOE1 ]
  ]

  ifelse (count potential-participants) > 0                                                                                                      ;; check if there are any companies left to add to my-participants, otherwise planning the asset is cancelled (the asset dies)
  [ let potential-participant one-of potential-participants                                                                                      ;; A16 a random participant is selected as potential participant
    let potential-new-market-risk (item current-position ([market-risk-list] of potential-participant))
    set production-asset-market-risk min (list potential-new-market-risk production-asset-market-risk)                                           ;; A17 if the potential participant has a lower sector market risk the production-asset-market-risk is updated to the lower value
    create-share-from potential-participant
    [ set share-percentage 0 ]
    set my-participants in-share-neighbors                                                                                                       ;; update my-participants to include the potential-participant
  ]
  [ ifelse production-asset-type = 0
    [ set reporter-planning-risk lput 1 reporter-planning-risk
    ]
    [ set reporter-planning-capital-intensity lput 2 reporter-planning-capital-intensity
    ]
    die
  ]

  let number-of-participants count my-participants                                                                                               ;; A28 shares are updated based on the number of participants and the standard distribution of shares
  let current-shares my-shares

  ifelse (SOE1-active? = true) and (SOE1-asset-investment = "mandatory-participation")
  [ set number-of-participants number-of-participants - count my-participants with [SOE1 = self]
    set current-shares my-shares with [end1 != SOE1]

    ask max-one-of current-shares [share-percentage]
    [ ask current-shares with [self != myself]
      [ set share-percentage item 1 (item (number-of-participants - 1) standard-shares) * (1 - participation-percentage)
      ]
      set share-percentage item 0 (item (number-of-participants - 1) standard-shares) * (1 - participation-percentage)                           ;; after the other shares have been set, the first share is updated as well
    ]
  ]
  [ ask max-one-of current-shares [share-percentage]
    [ ask current-shares with [self != myself]
      [ set share-percentage item 1 (item (number-of-participants - 1) standard-shares)
      ]
      set share-percentage item 0 (item (number-of-participants - 1) standard-shares)                                                            ;; after the other shares have been set, the first share is updated as well
    ]
  ]

end


to set-production-technology

  let asset-available-technologies []                                                                                                            ;; A21 participants will evaluate all those technologies available to them based on technology risk and expected hydrogen production costs
  ask my-participants
  [ foreach company-available-technologies                                                                                                       ;; all the technology types available to a participant are selected and added to a list
    [ x ->
      set asset-available-technologies lput x asset-available-technologies
    ]
  ]
  set asset-available-technologies remove-duplicates asset-available-technologies                                                                ;; select the unique technology types of the list, resulting in the technologies available to my-participants

  ;show asset-available-technologies

  let average-risk-appetite (mean ([risk-appetite] of my-participants))                                                                          ;; A25 risk appetite is used to select the technology

  let technologies-CAPEX []                                                                                                                      ;; necessary to keep track of all the elements that have learning-by-doing (for which technology specificatons for participants differ)
  let technologies-OPEX []                                                                                                                       ;; necessary to keep track of all the elements that have learning-by-doing (for which technology specificatons for participants differ)
  let technologies-risk []                                                                                                                       ;; this list will store the technology risk for each available technology
  let technologies-expected-CAPEX []                                                                                                             ;; necessary to keep track of all the elements that are influenced by technology risk (for which the perceived value during planning differs from the actual construciton value)
  let technologies-expected-efficiency []                                                                                                        ;; necessary to keep track of all the elements that are influenced by technology risk (for which the perceived value during planning differs from the actual construciton value)
  let technologies-expected-costs []                                                                                                             ;; this list will store the expected hydrogen production costs for each available technology

  foreach asset-available-technologies                                                                                                           ;; loop through all the available technologies
  [ x ->

    let participants-CAPEX []
    let participants-OPEX []
    let participants-technology-risk []                                                                                                          ;; this list will contain the technology risk of each participant for the current technology in the loop

    let participants-expected-CAPEX []
    let participants-expected-efficiency []
    let participants-expected-costs []                                                                                                           ;; this list will contain the expected costs of each participant for the current technology in the loop

    let force-sustainability? false
    if SOE1-active? = true
      [ if (SOE1-sustainable? = true) and (member? SOE1 my-participants)
        [ set force-sustainability? true ]
    ]

    ask my-participants                                                                                                                          ;; each participant must look up the specifications for its own technologies
    [ let current-company-technology item 0 filter [ y -> item technology-type y = x ] company-technologies                                      ;; select the specification for the current technology in the loop from the list containing the companies technology lists, e.g. x is "SMR", look for the technology list which has "SMR" as technology-type

      let current-CAPEX                         item technology-CAPEX              current-company-technology
      let current-OPEX                          item technology-OPEX               current-company-technology
      let current-efficiency                    item technology-efficiency         current-company-technology
      let current-technology-risk               item technology-risk               current-company-technology
      let current-technology-lifetime           item technology-lifetime           current-company-technology
      let current-technology-emission-factor    item technology-emission-factor    current-company-technology
      let current-technology-emission-factor-0  item technology-emission-factor-0  current-company-technology

      let current-expected-CAPEX                max (list 0 (random-normal current-CAPEX      (current-technology-risk * current-CAPEX)))        ;; A22 each participant expects the CAPEX and efficiency to be of a certain value based on the technology risk
      let current-expected-efficiency           min (list 1 (random-normal current-efficiency (current-technology-risk * current-efficiency)))   ;; A22 each participant expects the CAPEX and efficiency to be of a certain value based on the technology risk

      let current-fixed-OPEX current-expected-CAPEX * current-OPEX                                                                               ;; €/MW * % / y = €/MW/y
      let current-energy-source-OPEX  0
      let current-sustainability-OPEX 0
      ifelse (x = "SMR") or (x = "ATR")
      [ set current-energy-source-OPEX natural-gas-price / current-expected-efficiency                                                           ;; €/MWh / % = €/MWh
        ifelse force-sustainability? = true
        [ set current-sustainability-OPEX (CCS-price * current-technology-emission-factor-0 + carbon-price * (1 - current-technology-emission-factor-0)) * current-technology-emission-factor
        ]
        [ ifelse (CCS-price * current-technology-emission-factor-0 + carbon-price * (1 - current-technology-emission-factor-0)) <= carbon-price
          [ set current-sustainability-OPEX (CCS-price * current-technology-emission-factor-0 + carbon-price * (1 - current-technology-emission-factor-0)) * current-technology-emission-factor ]   ;; €/ton CO2 * ton CO2/MWh = €/MWh
          [ set current-sustainability-OPEX carbon-price * current-technology-emission-factor ]                                                  ;; €/ton CO2 * ton CO2/MWh = €/MWh
        ]
      ]
      [ set current-energy-source-OPEX electricity-price / current-expected-efficiency                                                           ;; €/MWh / % = €/MWh
        ifelse force-sustainability? = true
        [ set current-sustainability-OPEX GO-price / current-expected-efficiency
        ]
        [ ifelse GO-price <= carbon-price
          [ set current-sustainability-OPEX GO-price / current-expected-efficiency ]                                                             ;; €/MWh / % = €/MWh
          [ set current-sustainability-OPEX carbon-price * current-technology-emission-factor / current-expected-efficiency ]                    ;; €/ton CO2 * ton CO2/MWh / % = €/MWh
        ]
      ]
      let current-variable-OPEX current-energy-source-OPEX + current-sustainability-OPEX

      let current-expected-costs ((current-expected-CAPEX / current-technology-lifetime + current-fixed-OPEX) / operating-hours + current-variable-OPEX)      ;; hydrogen production costs in €/MWh are used to compare technologies because comparing only CAPEX would be insufficient due to differences in efficiency between technologies

      set participants-CAPEX                 lput current-CAPEX                participants-CAPEX                                                ;; in €/MW
      set participants-OPEX                  lput current-OPEX                 participants-OPEX                                                 ;; in %
      set participants-technology-risk       lput current-technology-risk      participants-technology-risk
      set participants-expected-CAPEX        lput current-expected-CAPEX       participants-expected-CAPEX                                       ;; in €/MW
      set participants-expected-efficiency   lput current-expected-efficiency  participants-expected-efficiency                                  ;; in %
      set participants-expected-costs        lput current-expected-costs       participants-expected-costs
    ]

    set participants-CAPEX                   mean participants-CAPEX
    set participants-OPEX                    mean participants-OPEX
    set participants-technology-risk         min  participants-technology-risk                                                                   ;; A24 the minimum technology risk for a technology of all participants is set as the final technology risk for the current technology in the loop
    set participants-expected-CAPEX          mean participants-expected-CAPEX
    set participants-expected-efficiency     mean participants-expected-efficiency
    set participants-expected-costs          mean participants-expected-costs                                                                    ;; A23 the expected costs for a technology from each participant is averaged to one value which is the final expected costs for the current technology in the loop

    let current-technology item 0 filter [ y -> item technology-type y = x ] technologies

    let current-technology-position position current-technology technologies                                                                     ;; update reporter for expected production costs of the technology
    let current-expected-production-costs item current-technology-position reporter-production-costs-list
    set current-expected-production-costs lput participants-expected-costs current-expected-production-costs
    set reporter-production-costs-list replace-item current-technology-position reporter-production-costs-list current-expected-production-costs

    ;show participants-technology-risk
    ;show production-asset-market-risk

    let current-TRL item technology-TRL current-technology
    ifelse ((precision (participants-technology-risk + production-asset-market-risk) 4) <= average-risk-appetite) and (current-TRL >= 7)         ;; A25 only those technologies that fall within the risk appetite are considered, A55 only TRL 7 or higher is invested in
    [ set technologies-CAPEX                 lput participants-CAPEX                technologies-CAPEX
      set technologies-OPEX                  lput participants-OPEX                 technologies-OPEX
      set technologies-risk                  lput participants-technology-risk      technologies-risk                                            ;; the final technology risk for the current technology in the loop is added to the list containing the technology risks for all available technologies
      set technologies-expected-CAPEX        lput participants-expected-CAPEX       technologies-expected-CAPEX
      set technologies-expected-efficiency   lput participants-expected-efficiency  technologies-expected-efficiency
      set technologies-expected-costs        lput participants-expected-costs       technologies-expected-costs                                  ;; the final expected-costs for the current technology in the loop is added to the list containing the expected-costs for all available technologies
    ]
    [ set asset-available-technologies remove x asset-available-technologies
    ]

  ]

  ;show asset-available-technologies

  ifelse empty? asset-available-technologies                                                                                                     ;; if no technology is left in the selection the asset is not planned
  [ set reporter-planning-suitable-technology lput 3 reporter-planning-suitable-technology
    die
  ]

  [ let min-expected-costs       min technologies-expected-costs                                                                                 ;; A25
    let technology-position      (position min-expected-costs technologies-expected-costs)                                                       ;; select the position of the technology with the lowest production costs

    let my-technology            item technology-position asset-available-technologies                                                           ;; select the name of technology first from available technologies, to make sure that the proper position is used
    let my-CAPEX                 item technology-position technologies-CAPEX
    let my-OPEX                  item technology-position technologies-OPEX
    let my-risk                  item technology-position technologies-risk
    let my-expected-CAPEX        item technology-position technologies-expected-CAPEX
    let my-expected-efficiency   item technology-position technologies-expected-efficiency

    ;show my-technology

    set my-production-technology item 0 filter [ x -> item technology-type x = my-technology ] [company-technologies] of one-of my-participants  ;; actually select the specifications of that technology

    set my-production-technology replace-item technology-CAPEX  my-production-technology my-CAPEX                                                ;; set the production-technology specifications using the spefications from my-participants
    set my-production-technology replace-item technology-OPEX   my-production-technology my-OPEX
    set my-production-technology replace-item technology-risk   my-production-technology my-risk

    set my-expected-production-technology my-production-technology                                                                               ;; set the production-technology expected specifications using the spefications from my-participants
    set my-expected-production-technology replace-item technology-CAPEX my-expected-production-technology my-expected-CAPEX
    set my-expected-production-technology replace-item technology-efficiency my-expected-production-technology my-expected-efficiency

    set production-asset-capacity item technology-capacity my-production-technology                                                              ;; these variables are fixed and not dependent on technology risk, which is why they can be assigned to the production asset here
    set production-asset-lifetime item technology-lifetime my-production-technology
    set production-asset-type     item technology-type my-production-technology
    set label production-asset-type

    ;show min-expected-costs

  ]

end


to set-sectors [my-potential-sectors my-potential-sectors-shortages]

  let production-asset-capacity-remaining (production-asset-capacity * operating-hours * PJ-per-MWh)                                             ;; denotes the production-asset-capacity that is not yet assigned to a sector in PJ
  let current-max-sector-shortage max my-potential-sectors-shortages
  let current-position position current-max-sector-shortage my-potential-sectors-shortages

  ifelse production-asset-capacity-remaining <= current-max-sector-shortage                                                                      ;; first check whether for the first sector already in my-sectors the expected shortage is large enough
  [ create-contract-with my-sectors
    [ set contract-status "planned"
      set contract-quantity (production-asset-capacity-remaining / PJ-per-MWh)                                                                   ;; in MWh/y
    ]
  ]
  [ set production-asset-capacity-remaining production-asset-capacity-remaining - current-max-sector-shortage
    create-contract-with my-sectors
    [ set contract-status "planned"
      set contract-quantity (current-max-sector-shortage / PJ-per-MWh)
    ]

    while [production-asset-capacity-remaining > 0]                                                                                              ;; if the expected shortage of the first sector is not large enough, the other sectors will iteratively be added
    [ set my-potential-sectors remove-item current-position my-potential-sectors                                                                 ;; the sector which has just been added to my-sectors is no longer available
      set my-potential-sectors-shortages remove-item current-position my-potential-sectors-shortages
      if empty? (filter [ i -> i > 0 ] my-potential-sectors-shortages)                                                                           ;; if there are no sectors left to add with an expected demand shortage that is positive, the asset will not be planned
      [ set reporter-planning-demand-shortage lput 0 reporter-planning-demand-shortage
        die
      ]

      ;show my-potential-sectors
      ;show my-potential-sectors-shortages
      ;show production-asset-capacity-remaining

      set current-max-sector-shortage max my-potential-sectors-shortages                                                                         ;; select the maximum in the list with expected shortages for all sectors
      set current-position position current-max-sector-shortage my-potential-sectors-shortages                                                   ;; position of the the maximum demand in the list with expected shortages for all sectors is retrieved

      ifelse production-asset-capacity-remaining <= current-max-sector-shortage                                                                  ;; check whether the additional sector has a large enough expected demand
      [ create-contract-with (item current-position my-potential-sectors)                                                                        ;; if the additional sector has a large enough expected demand, the remaining quantity is set to 0
        [ set contract-status "planned"
          set contract-quantity (production-asset-capacity-remaining / PJ-per-MWh )
        ]
        set production-asset-capacity-remaining 0
      ]
      [ create-contract-with (item current-position my-potential-sectors)                                                                        ;; if the additional sector does not have a large enough expected demand, the remaining quantity is adjusted
        [ set contract-status "planned"
          set contract-quantity (current-max-sector-shortage / PJ-per-MWh )
        ]
        set production-asset-capacity-remaining production-asset-capacity-remaining - current-max-sector-shortage
      ]
    ]
  ]

  set my-sectors contract-neighbors

  ask my-contracts
  [ set thickness contract-quantity / 1E6
  ]

  set-location

end


to set-location

  ask max-one-of my-contracts [contract-quantity]                                                                                                ;; A26 distance to the first sector is smaller than for the other sectors, first sector is the sector with the largest contracted quantity
  [ set contract-distance random-float (max-first-sector-distance - min-first-sector-distance) + min-first-sector-distance                       ;; A26 with random float
    set color white
  ]

  ask my-contracts with [self != [max-one-of my-contracts [contract-quantity]] of myself]                                                        ;; A27 distance to the other sectors is further away than distance to the first sector
  [ set contract-distance random-float (max-other-sectors-distance - min-other-sectors-distance) + min-other-sectors-distance                    ;; A27 with random float
    set color white
  ]

  setxy ((random (2 * world-size)) - world-size) ((random (2 * world-size)) - world-size) ; this location spawning influences final location through layoutspring but is necessary!

                           ;ask production-assets, question is if an iteration of all production assets is good to do? to make sure they repel eachother
                           ;iterate over production assets and contracts to allow heterogeneous springs that match distances of contracts RESULT IS STILL MISLEADING if sectors are on opposite sides

  repeat 30
  [ ask my-contracts
    [ let current-agentset (turtle-set myself)
      let current-link-set (link-set self)
      layout-spring current-agentset current-link-set 0.1 (contract-distance * (world-size * 2) / max-other-sectors-distance) 10
    ]
  ]

end


to set-total-CAPEX [current-production-technology]                                                                                               ;; current-production-technology is used to either evaluate total-expected-CAPEX or total-CAPEX

  let total-production-CAPEX ((item technology-CAPEX current-production-technology) * production-asset-capacity)                                 ;; in €

  ask my-contracts                                                                                                                               ;; A45 each sector quantity and distance is evaluated seperately
  [ ifelse policy-public-infrastructure-active? = true
    [ set my-transport-technology     "public-infrastructure"
      set contract-transport-OPEX     0
      set contract-transport-CAPEX    0
    ]
    [ let current-capacity contract-quantity / operating-hours                                                                                   ;; A46 pipelines are sized for max capacity, this allows for overcapacity
      ifelse (policy-blending-active? = true) and (policy-blending-capacity-available >= current-capacity)
      [ set my-transport-technology   "blending"
        set contract-transport-OPEX   0
        set contract-transport-CAPEX  0
      ]
      [ let diameter-pipeline (2 * sqrt (current-capacity / ((pipelines-hydrogen-density * 10 ^ -9) * (pipelines-hydrogen-velocity * 10 ^ 3) * pi * LHV-hydrogen)))  ;; A66 formula given after consultation from Mulder et al. (2019), relevant units converted to millimeters
        let pipelines-CAPEX pipelines-beta3 + pipelines-beta2 * diameter-pipeline + pipelines-beta1 * diameter-pipeline ^ 2                      ;; A65 assumed that dedicated hydrogen infrastructure will be realized, formula given by Mulder et al. (2019) in €/m
        set pipelines-CAPEX contract-distance * 1000 * pipelines-CAPEX                                                                           ;; in €

        let number-of-trucks       ceiling (current-capacity / (trucks-speed / (2 * contract-distance) * trucks-capacity * MWh-per-kg))          ;; in #trucks, A50 number of trucks needed is based on the average speed of a single truck, distance is multiplied by 2 because empty trucks have to drive back from the demand sector
        let total-trucks-CAPEX     trucks-CAPEX * number-of-trucks                                                                               ;; in €, A47 lifetime of trucks is not taken into account
        let total-trucks-OPEX      (2 * contract-distance * trucks-OPEX) / (trucks-capacity * MWh-per-kg) * contract-quantity
        set total-trucks-OPEX      total-trucks-OPEX * [production-asset-lifetime] of end2                                                       ;; in €

        ifelse (total-trucks-OPEX + total-trucks-CAPEX) < pipelines-CAPEX                                                                        ;; A48 pipeline-OPEX is not taken into account
        [ set my-transport-technology "trucks"
          set contract-transport-OPEX  trucks-OPEX * 2 * contract-distance                                                                       ;; in €/truck
          set contract-transport-OPEX  contract-transport-OPEX / (trucks-capacity * MWh-per-kg)                                                  ;; in €/MWh, A51 flexible costs for €/MWh are assumed so a truck driving with reduced capacity will pay reduced OPEX instead of full OPEX
          set contract-transport-CAPEX total-trucks-CAPEX
          set shape "trucks"
        ]
        [ set my-transport-technology "pipelines"
          set contract-transport-OPEX  0                                                                                                         ;; in €/MWh
          set contract-transport-CAPEX pipelines-CAPEX                                                                                           ;; in €
        ]
      ]
    ]
  ]

  let total-transport-CAPEX sum [contract-transport-CAPEX] of my-contracts

  set production-asset-total-CAPEX (total-production-CAPEX + total-transport-CAPEX)                                                              ;; in €

end


to update-my-production-technologies

  let old-CAPEX                 item technology-CAPEX      my-production-technology
  let old-OPEX                  item technology-OPEX       my-production-technology
  let old-technology-risk       item technology-risk       my-production-technology
  let old-expected-CAPEX        item technology-CAPEX      my-expected-production-technology
  let old-expected-efficiency   item technology-efficiency my-expected-production-technology

  let new-CAPEX                 0
  let new-OPEX                  0
  let new-technology-risk       0
  let new-expected-CAPEX        0
  let new-expected-efficiency   0

  ask my-participants with [ not member? myself my-production-assets ]
  [ let x [production-asset-type] of myself
    let new-company-technology item 0 filter [ y -> item technology-type y = x ] company-technologies

    set new-CAPEX                      item technology-CAPEX            new-company-technology
    set new-OPEX                       item technology-OPEX             new-company-technology
    let new-efficiency                 item technology-efficiency       new-company-technology
    set new-technology-risk            item technology-risk             new-company-technology

    set new-expected-CAPEX             max (list 0 (random-normal new-CAPEX          (new-technology-risk * new-CAPEX)))
    set new-expected-efficiency        min (list 1 (random-normal new-efficiency     (new-technology-risk * new-efficiency)))
  ]

  set old-CAPEX               old-CAPEX *               (count my-participants - 1) / (count my-participants)                                     ;; because the average specifications are taken the old and new specifications are weighted based on the number of participants
  set old-OPEX                old-OPEX *                (count my-participants - 1) / (count my-participants)
  set old-expected-CAPEX      old-expected-CAPEX *      (count my-participants - 1) / (count my-participants)
  set old-expected-efficiency old-expected-efficiency * (count my-participants - 1) / (count my-participants)

  set new-CAPEX               new-CAPEX *               (1) / (count my-participants) + old-CAPEX
  set new-OPEX                new-OPEX *                (1) / (count my-participants) + old-OPEX
  set new-technology-risk     min (list new-technology-risk old-technology-risk)                                                                  ;; only for technology risk the value is not averaged, but the minimum value is used
  set new-expected-CAPEX      new-expected-CAPEX *      (1) / (count my-participants) + old-expected-CAPEX
  set new-expected-efficiency new-expected-efficiency * (1) / (count my-participants) + old-expected-efficiency

  set my-production-technology          replace-item technology-CAPEX      my-production-technology          new-CAPEX
  set my-production-technology          replace-item technology-OPEX       my-production-technology          new-OPEX
  set my-production-technology          replace-item technology-risk       my-production-technology          new-technology-risk
  set my-expected-production-technology replace-item technology-CAPEX      my-expected-production-technology new-expected-CAPEX
  set my-expected-production-technology replace-item technology-efficiency my-expected-production-technology new-expected-efficiency

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execution phase ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to update-prices

  if constant-externals? = false
  [ set natural-gas-price             item ticks natural-gas-price-list
    set electricity-price             item ticks electricity-price-list
    set ETS-price                     item ticks ETS-price-list
    set GO-price                      item ticks GO-price-list
    set CCS-price                     item ticks CCS-price-list
    set electricity-emission-factor   item ticks electricity-emission-factor-list

    set carbon-price                  max (list ETS-price policy-carbon-tax)
  ]

  ;show natural-gas-price
  ;show electricity-price
  ;show ETS-price
  ;show GO-price
  ;show ticks
  ;show CCS-price
  ;show electricity-emission-factor
  ;show carbon-price

end


to companies-do-knowledge-investments

  ask companies                                                                                                                                  ;; effect of investments done by companies in knowledge project on market and technology risk is calculated here
  [ (foreach market-risk-list initial-market-risk-list sector-cumulative-knowledge
     [ [risk initial-risk cumulative-investment] ->

       let current-position position risk market-risk-list
       if cumulative-investment >= knowledge-costs-factor                                                                                        ;; necessary to check if a minimum has been invested, if not the risk would increase when using the given formula
       [ set risk precision (initial-risk * ( (cumulative-investment / knowledge-costs-factor) ^ (- knowledge-factor) )) 4
         set market-risk-list replace-item current-position market-risk-list risk
       ]

     ]
    )

    let i 0
    while [ i < length technologies ]
    [ let current-company-technology    item i company-technologies
      let cumulative-investment         item technology-cumulative-knowledge current-company-technology
      let initial-risk                  item i initial-technology-risk-list

      if cumulative-investment >= knowledge-costs-factor
      [ let risk                        precision (initial-risk * ( (cumulative-investment / knowledge-costs-factor) ^ (- knowledge-factor) )) 4
        set current-company-technology  replace-item technology-risk current-company-technology risk
        set company-technologies        replace-item i company-technologies current-company-technology
      ]

      set i i + 1
    ]


    ;show sector-cumulative-knowledge
    ;show initial-market-risk-list
    ;show market-risk-list
    ;show company-technologies
    ;show initial-technology-risk-list

  ]

end


to companies-do-asset-investments

  ask production-assets with [production-asset-status = "planned"]                                                                               ;; all the production-assets that were planned will start construction with their actual specifications
  [ set-total-CAPEX                             my-production-technology                                                                         ;; the actual investment costs are calculated

    set production-asset-type                   production-asset-type
    ifelse production-asset-type = "SMR" or production-asset-type = "ATR"
    [ set energy-source                         "natural-gas"
    ]
    [ set energy-source                         "electricity"
    ]
    set production-asset-status                 "constructing"
    set production-asset-sustainable?           false
    set production-asset-capacity               production-asset-capacity
    set production-asset-fixed-OPEX             item technology-CAPEX my-production-technology * item technology-OPEX my-production-technology * production-asset-capacity
    set production-asset-efficiency             item technology-efficiency my-production-technology
    set production-asset-lifetime               production-asset-lifetime
    set production-asset-construction-time      item technology-construction-time my-production-technology
    set production-asset-TRL                    item technology-TRL my-production-technology
    set production-asset-emission-factor        item technology-emission-factor my-production-technology
    set production-asset-emission-factor-0      item technology-emission-factor-0 my-production-technology
    set production-asset-technology-risk        item technology-risk my-production-technology
    set construction-first-year                 ticks
    set construction-last-year                  ticks + production-asset-construction-time - 1                                                   ;; - 1 necessary to allow production started at the beginning of a year to be finished at the end
    set production-asset-ROI                    standard-ROI + production-asset-market-risk + production-asset-technology-risk                   ;; A75 ROI is set for each production asset meaning that there is no company specific effect added to the ROI
    set color                                   grey

    ask my-shares                                                                                                                                ;; company budget and technologies are updated due to the construction of a new asset
    [ let current-share-percentage              share-percentage
      let current-total-CAPEX                   [ production-asset-total-CAPEX ] of end2                                                         ;; end2 of shares is the production asset
      let current-technology-type               [ production-asset-type        ] of end2
      let added-capacity                        [ production-asset-capacity    ] of end2

      ask end1                                                                                                                                   ;; end1 of shares are the companies, these need to update their total-budget, their cumulative installed capacity and their CAPEX
      [ set total-budget                        total-budget - current-share-percentage * current-total-CAPEX
        set cumulative-asset-investments        cumulative-asset-investments + current-share-percentage * current-total-CAPEX

        let current-company-technology          item 0 filter [ y -> item technology-type y = current-technology-type ] company-technologies     ;; this procedure updates the company technology for CAPEX and cumulative installed capacity
        let current-technology                  item 0 filter [ y -> item technology-type y = current-technology-type ] technologies
        let current-position                    position current-company-technology company-technologies

        let current-capacity                    item technology-cumulative-capacity current-company-technology                                   ;; first the company adds the newly installed capacity to the cumulative installed capacity for the current technology
        set current-capacity                    current-capacity + added-capacity                                                                ;; A71 full capacity is added to cumulative installed capacity for all participants
        let current-initial-CAPEX               item technology-CAPEX current-technology                                                         ;; then the company updates the CAPEX for the technology
        let current-learning-by-doing           item technology-learning-by-doing current-company-technology
        let current-CAPEX                       current-initial-CAPEX * (( current-capacity / added-capacity ) ^ (- current-learning-by-doing) ) ;; added-capacity is the base capacity, similar to knowledge-costs-factor, which is constant

        set current-company-technology          replace-item technology-cumulative-capacity current-company-technology current-capacity          ;; then the company updates its technologies list
        set current-company-technology          replace-item technology-CAPEX current-company-technology current-CAPEX
        set company-technologies                replace-item current-position company-technologies current-company-technology

        ;show current-share-percentage * current-total-CAPEX
        ;show total-budget

      ]

      if hide-shares? = true                                                                                                                     ;; visual procedure
      [ set hidden? true
      ]

    ]

    ask my-contracts
    [ set contract-status "constructing"
      if my-transport-technology = "blending"                                                                                                    ;; A76 only when the asset (and therefore its transport) is constructed, is blending capacity claimed, overcapacity from the marginal production asset is allowed
      [ let current-capacity contract-quantity / operating-hours
        set policy-blending-capacity-available policy-blending-capacity-available - current-capacity
      ]
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operation phase ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to companies-operate-assets

  ask production-assets with [production-asset-status != "constructing"]                                                                         ;; all production-assets that have finished constructing evaluate their revenue, operational costs and profit
  [ determine-variable-OPEX
    let production-asset-variable-OPEX   ( energy-source-OPEX + sustainability-OPEX + transport-OPEX)                                            ;; in €/MWh
    set production-this-year             sum ([contract-quantity] of (my-contracts with [contract-status = "accepted"]))                         ;; in MWh
    set production-asset-total-OPEX      ( production-asset-fixed-OPEX + production-asset-variable-OPEX * production-this-year)                  ;; in €, A8 production at reduced capacity also reduces the variable operational costs
    set production-asset-revenue         sum ( [(contract-quantity * contract-price)] of (my-contracts with [contract-status = "accepted"]))
    set production-asset-profit          production-asset-revenue - production-asset-total-OPEX

    ask my-shares
    [ set share-profit ([production-asset-profit] of end2 * share-percentage)
    ]

    set production-asset-cumulative-profit production-asset-cumulative-profit + production-asset-profit

    ifelse energy-source = "natural-gas"
    [ ifelse production-asset-sustainable? = true
      [ set carbon-emissions production-this-year * production-asset-emission-factor * (1 - production-asset-emission-factor-0)
        set carbon-captured  production-this-year * production-asset-emission-factor * production-asset-emission-factor-0
      ]
      [ set carbon-emissions production-this-year * production-asset-emission-factor
      ]
    ]
    [ ifelse production-asset-sustainable? = true
      [ set carbon-emissions 0
        set green-electricity-consumption production-this-year / production-asset-efficiency
      ]
      [ set carbon-emissions production-this-year * production-asset-emission-factor /  production-asset-efficiency
      ]
    ]
  ]

  ask companies
  [ set total-budget total-budget + sum ([share-profit] of my-shares)
    set cumulative-asset-profit cumulative-asset-profit + sum ([share-profit] of my-shares)

    ;ask my-shares [show share-profit]
    ;show total-budget
  ]

  update-reporters

  ask production-assets with [production-asset-status = "operating"]                                                                             ;; assets which are operating have active contracts and should check when these expire, if they expire the asset sets status to idle
  [ ask my-contracts
    [ if ticks = contract-last-year
      [ set contract-status "idle"
        set color red
        ask end2
        [ set production-asset-status "idle"
        ]
      ]
    ]
  ]

  ask production-assets with [production-asset-status != "constructing"]                                                                         ;; all assets are planned within a year, they then become constructing and do not age yet, all other assets should age and die at end-of-life
  [ set production-asset-age production-asset-age + 1
    if production-asset-age = production-asset-lifetime
    [ die ]
  ]

  ask production-assets with [production-asset-status = "idle"]                                                                                  ;; A5 disinvestment happens only if the asset is idle for a number of years, so not operating at all
  [ set years-idle years-idle + 1
    if years-idle = 4
    [ die ]
  ]

  ask production-assets with [production-asset-status = "constructing"]                                                                          ;; any assets that have finished construction will be set in the idle state - awaiting contract proposal and acceptance
  [ if ticks = construction-last-year
    [ ask my-contracts
      [ set contract-status "idle"
        set color red
      ]
      set production-asset-status "idle"

      let current-technology production-asset-type                                                                                               ;; update the cumulative installed capacity for the technology of the production asset that has just started construction
      set current-technology item 0 (filter [ x -> production-asset-type = item technology-type x ] technologies)
      let current-technology-index position current-technology technologies
      let current-technology-cumulative-capacity item technology-cumulative-capacity current-technology
      set current-technology-cumulative-capacity current-technology-cumulative-capacity + production-asset-capacity
      set current-technology replace-item technology-cumulative-capacity current-technology current-technology-cumulative-capacity
      set technologies replace-item current-technology-index technologies current-technology
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reporter procedures  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-reporters

  ifelse empty? (item technology-SMR reporter-production-costs-list )
  [ set reporter-production-costs-SMR      reporter-production-costs-SMR
  ]
  [ set reporter-production-costs-SMR      mean (item technology-SMR  reporter-production-costs-list) * MWh-per-kg
  ]
  ifelse empty? (item technology-ATR reporter-production-costs-list )
  [ set reporter-production-costs-ATR      reporter-production-costs-ATR
  ]
  [ set reporter-production-costs-ATR      mean (item technology-ATR  reporter-production-costs-list) * MWh-per-kg
  ]
   ifelse empty? (item technology-ALK reporter-production-costs-list )
  [ set reporter-production-costs-ALK      reporter-production-costs-ALK
  ]
  [ set reporter-production-costs-ALK      mean (item technology-ALK  reporter-production-costs-list) * MWh-per-kg
  ]
   ifelse empty? (item technology-PEM reporter-production-costs-list )
  [ set reporter-production-costs-PEM      reporter-production-costs-PEM
  ]
  [ set reporter-production-costs-PEM      mean (item technology-PEM  reporter-production-costs-list) * MWh-per-kg
  ]
   ifelse empty? (item technology-SOEC reporter-production-costs-list )
  [ set reporter-production-costs-SOEC     reporter-production-costs-SOEC
  ]
  [ set reporter-production-costs-SOEC     mean (item technology-SOEC reporter-production-costs-list) * MWh-per-kg
  ]
  set reporter-production-costs-list       (list [] [] [] [] [])

  ask companies
  [ set assets-participating                                  count my-production-assets
    set assets-participating-production                       precision (sum ([production-this-year] of my-production-assets)) 0
    let my-shares-production                                  0
    ask my-shares
    [ set my-shares-production                                my-shares-production + share-percentage * ([production-this-year] of end2)
    ]
    set assets-participating-production-share                 precision (my-shares-production) 0
  ]

  set reporter-total-assets                                   count production-assets
  set reporter-total-demand                                   sum ([demand] of sectors)
  set reporter-total-production-contracted                    sum ([sector-contracted-production] of sectors)
  set reporter-total-production-capacity                      sum [production-asset-capacity] of (production-assets with [production-asset-status != "constructing"]) * operating-hours * PJ-per-MWh

  set reporter-total-carbon-emissions                         sum [carbon-emissions] of production-assets
  set reporter-total-cumulative-carbon-emissions              reporter-total-cumulative-carbon-emissions + reporter-total-carbon-emissions

  set reporter-total-carbon-captured                          sum [carbon-captured] of production-assets / 1000
  set reporter-total-cumulative-carbon-captured               reporter-total-cumulative-carbon-captured + reporter-total-carbon-captured

  set reporter-total-green-electricity-consumption            sum [green-electricity-consumption] of production-assets * PJ-per-MWh

  let total-production-MWh                                    reporter-total-production-contracted / PJ-per-MWh
  set reporter-average-hydrogen-price                         sum ([contract-price * contract-quantity / total-production-MWh] of (contracts with [contract-status = "accepted"]))
  set reporter-average-hydrogen-price                         reporter-average-hydrogen-price * MWh-per-kg

  ifelse total-production-MWh  > 0
  [ set reporter-average-hydrogen-carbon-intensity  sum       ([carbon-emissions] of production-assets) / total-production-MWh
  ]
  [ set reporter-average-hydrogen-carbon-intensity 0
  ]

  ifelse SOE1-active? = true
  [ set reporter-cumulative-investment-SOE1                   ([cumulative-asset-investments] of SOE1) / 1E6 + ([cumulative-knowledge-investments] of SOE1) / 1E6
    set reporter-cumulative-profit-SOE1                       ([cumulative-asset-profit] of SOE1) / 1E6
    ifelse reporter-total-production-contracted > 0
    [ set reporter-production-participant-SOE1                precision ( ([assets-participating-production] of SOE1) * PJ-per-MWh / reporter-total-production-contracted ) 5
      set reporter-production-share-SOE1                      precision ( ([assets-participating-production-share] of SOE1) * PJ-per-MWh / reporter-total-production-contracted ) 5
    ]
    [ set reporter-production-participant-SOE1                0
      set reporter-production-share-SOE1                      0
    ]
  ]
  [ set reporter-cumulative-investment-SOE1                   0
    set reporter-cumulative-profit-SOE1                       0
    set reporter-production-participant-SOE1                  0
    set reporter-production-share-SOE1                        0
  ]

  let max-cumulative-capacity                                 max (map [ i -> item technology-cumulative-capacity i ] technologies)
  ifelse max-cumulative-capacity = 0
  [ set reporter-dominant-technology                          "None"
  ]
  [ set reporter-dominant-technology                          item technology-type (item 0 (filter [ i -> item technology-cumulative-capacity i = max-cumulative-capacity] technologies))
  ]

  ifelse reporter-total-demand = 0
  [ set reporter-relative-demand-shortage                     0
  ]
  [ set reporter-relative-demand-shortage                     (reporter-total-demand - reporter-total-production-contracted) / reporter-total-demand
  ]

  ifelse reporter-total-production-capacity = 0
  [ set reporter-capacity-factor                              0
  ]
  [ set reporter-capacity-factor                              reporter-total-production-contracted / reporter-total-production-capacity
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plots procedures  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to renew-plots

  set-current-plot "production-and-demand-per-sector"
  ask sectors
  [ create-temporary-plot-pen ( word "demand " self )
    set-plot-pen-color color
    plot demand
  ]

  ask sectors
  [ create-temporary-plot-pen ( word "production " self )
    set-plot-pen-color color + 3
    plot sector-contracted-production
  ]

  set-current-plot "demand-shortage-per-sector"
  ask sectors
  [ create-temporary-plot-pen ( word "shortage " self )
    set-plot-pen-color color
    plot (demand - sector-contracted-production)
  ]

  create-temporary-plot-pen ( word "base" )
  set-plot-pen-color black
  plot 0

  set-current-plot "average-hydrogen-price"
  create-temporary-plot-pen ( word "price" )
  set-plot-pen-color black
  plot reporter-average-hydrogen-price

  set-current-plot "average-hydrogen-carbon-intensity"
  create-temporary-plot-pen ( word "carbon-intensity" )
  set-plot-pen-color black
  plot reporter-average-hydrogen-carbon-intensity

  set-current-plot "average-expected-production-costs"
  create-temporary-plot-pen ( word "SMR" )
  set-plot-pen-color red
  plot reporter-production-costs-SMR

  create-temporary-plot-pen ( word "ATR" )
  set-plot-pen-color blue
  plot reporter-production-costs-ATR

  create-temporary-plot-pen ( word "ALK" )
  set-plot-pen-color yellow
  plot reporter-production-costs-ALK

  create-temporary-plot-pen ( word "PEM" )
  set-plot-pen-color green
  plot reporter-production-costs-PEM

  create-temporary-plot-pen ( word "SOEC" )
  set-plot-pen-color magenta
  plot reporter-production-costs-SOEC

  set-current-plot "asset-planning-successfulness"
  create-temporary-plot-pen ( word "demand shortage" )
  set-plot-pen-color red - 2
  set-plot-pen-mode 1
  histogram reporter-planning-demand-shortage

  create-temporary-plot-pen ( word "risk" )
  set-plot-pen-color red
  set-plot-pen-mode 1
  histogram reporter-planning-risk

  create-temporary-plot-pen ( word "capital intensity" )
  set-plot-pen-color orange
  set-plot-pen-mode 1
  histogram reporter-planning-capital-intensity

  create-temporary-plot-pen ( word "suitable technology" )
  set-plot-pen-color yellow
  set-plot-pen-mode 1
  histogram reporter-planning-suitable-technology

  create-temporary-plot-pen ( word "success" )
  set-plot-pen-color green
  set-plot-pen-mode 1
  histogram reporter-planning-success

  set-current-plot "total-production"
  create-temporary-plot-pen ( word "total capacity" )
  set-plot-pen-color red
  plot reporter-total-production-capacity

  create-temporary-plot-pen ( word "total contracted" )
  set-plot-pen-color black
  plot reporter-total-production-contracted

  if SOE1-active? = true
  [ create-temporary-plot-pen ( word "SOE1 participant" )
    set-plot-pen-color blue
    plot ([assets-participating-production] of SOE1) * PJ-per-MWh

    create-temporary-plot-pen ( word "SOE1 share" )
    set-plot-pen-color sky
    plot ([assets-participating-production-share] of SOE1) * PJ-per-MWh
  ]


end
@#$#@#$#@
GRAPHICS-WINDOW
683
40
1203
561
-1
-1
1.277
1
10
1
1
1
0
0
0
1
-200
200
-200
200
0
0
1
ticks
30.0

SWITCH
239
503
446
536
same-seed
same-seed
1
1
-1000

BUTTON
685
568
773
605
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
238
541
447
601
seed
101.0
1
0
Number

BUTTON
866
568
954
606
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
775
568
864
605
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1209
40
1588
285
production-and-demand-per-sector
years
PJ
0.0
30.0
0.0
400.0
false
true
"" ""
PENS

SLIDER
459
119
669
152
max-energy-demand
max-energy-demand
initial-demand
1500
58.0
1
1
PJ
HORIZONTAL

SLIDER
458
159
668
192
slope-energy-demand
slope-energy-demand
0
1
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
458
199
668
232
max-industry-demand
max-industry-demand
initial-demand
1500
650.0
1
1
PJ
HORIZONTAL

SLIDER
458
238
668
271
slope-industry-demand
slope-industry-demand
0
1
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
459
40
670
73
max-built-environment-demand
max-built-environment-demand
initial-demand
1500
180.0
1
1
PJ
HORIZONTAL

SLIDER
459
80
669
113
slope-built-environment-demand
slope-built-environment-demand
0
1
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
458
278
669
311
max-transport-demand
max-transport-demand
initial-demand
1500
63.0
1
1
PJ
HORIZONTAL

SLIDER
458
318
669
351
slope-transport-demand
slope-transport-demand
0
1
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
17
122
224
155
number-of-companies
number-of-companies
0
20
8.0
1
1
NIL
HORIZONTAL

SLIDER
460
458
667
491
participation-percentage
participation-percentage
0.01
0.99
0.4
0.01
1
NIL
HORIZONTAL

TEXTBOX
547
385
697
405
Levers
11
0.0
1

TEXTBOX
83
15
232
33
Input parameters
11
0.0
1

SWITCH
16
529
223
562
SOE1-sustainable?
SOE1-sustainable?
0
1
-1000

SLIDER
460
497
667
530
individual-knowledge
individual-knowledge
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
460
533
667
566
shared-knowledge
shared-knowledge
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
460
571
668
604
publish-knowledge
publish-knowledge
0
1
0.0
0.01
1
NIL
HORIZONTAL

CHOOSER
460
408
667
453
SOE1-asset-investment
SOE1-asset-investment
"no-asset-investment" "mandatory-participation" "non-mandatory-participation" "initiation"
0

SWITCH
961
569
1083
602
hide-shares?
hide-shares?
0
1
-1000

SWITCH
16
568
223
601
SOE2-active?
SOE2-active?
0
1
-1000

PLOT
1209
292
1589
512
demand-shortage-per-sector
years
PJ
0.0
30.0
-100.0
300.0
false
true
"" ""
PENS

PLOT
1900
292
2170
510
average-hydrogen-price
years
€/kg
0.0
30.0
0.0
5.0
false
false
"" ""
PENS

PLOT
1900
40
2169
284
average-hydrogen-carbon-intensity
years
ton CO2/MWh
0.0
30.0
0.0
0.5
false
false
"" ""
PENS

SWITCH
236
269
446
302
public-infrastructure-available?
public-infrastructure-available?
1
1
-1000

SWITCH
237
307
447
340
blending-available?
blending-available?
1
1
-1000

TEXTBOX
316
401
466
419
Model run\n
11
0.0
1

SWITCH
236
344
446
377
carbon-tax-available?
carbon-tax-available?
1
1
-1000

PLOT
1597
292
1892
511
average-expected-production-costs
years
€/kg
0.0
30.0
0.0
5.0
false
true
"" ""
PENS

PLOT
1210
703
1637
1101
asset-planning-successfulness
outcome of planning process
number of assets
0.0
5.0
0.0
10.0
true
true
"" ""
PENS

MONITOR
1455
566
1630
611
total demand [PJ]
reporter-total-demand
2
1
11

MONITOR
1634
516
1803
561
total contracted production [PJ]
reporter-total-production-contracted
2
1
11

PLOT
1597
40
1891
285
total-production
years
PJ
0.0
30.0
0.0
600.0
false
true
"" ""
PENS

MONITOR
1209
517
1448
562
SOE1 cumulative asset investments [M€]
([cumulative-asset-investments] of SOE1) / 1E6
2
1
11

MONITOR
1962
565
2170
610
cumulative carbon captured [kton CO2]
reporter-total-cumulative-carbon-captured
0
1
11

SLIDER
237
40
444
73
max-natural-gas-price
max-natural-gas-price
0
150
20.0
1
1
€/MWh
HORIZONTAL

TEXTBOX
286
15
474
38
External parameters
11
0.0
1

SLIDER
237
79
444
112
max-electricity-price
max-electricity-price
0
150
40.0
1
1
€/MWh
HORIZONTAL

SLIDER
237
117
445
150
max-ETS-price
max-ETS-price
0
300
25.0
1
1
€/ton CO2
HORIZONTAL

SLIDER
237
153
445
186
max-GO-price
max-GO-price
0
50
2.0
0.5
1
€/MWh
HORIZONTAL

SLIDER
237
190
445
223
max-CCS-price
max-CCS-price
0
300
40.0
1
1
€/ton CO2
HORIZONTAL

SLIDER
237
229
445
262
min-electricity-emission-factor
min-electricity-emission-factor
0
1
0.572
0.001
1
ton CO2/MWh
HORIZONTAL

BUTTON
238
425
447
460
base case
set max-natural-gas-price            20\nset max-electricity-price            40\nset max-ETS-price                    25\nset max-GO-price                     2\nset max-CCS-price                    40\nset min-electricity-emission-factor  0.572\nset public-infrastructure-available? false\nset blending-available?              false\nset carbon-tax-available?            false\nset max-built-environment-demand     180\nset slope-built-environment-demand   0.25\nset max-energy-demand                58\nset slope-energy-demand              0.25\nset max-industry-demand              650\nset slope-industry-demand            0.25\nset max-transport-demand             63\nset slope-transport-demand           0.25\nset number-of-companies              8\nset companies-risk-appetite-mean     0.045\nset SOE1-risk-appetite-increase      0.030\nset SOE1-asset-investment            \"no-asset-investment\"\nset participation-percentage         0.40\nset individual-knowledge             0.00\nset shared-knowledge                 0.00\nset publish-knowledge                0.00\nset knowledge-factor                 0.10\nset learning-by-doing-factor         0.10\nset companies-total-budget-mean      525\nset companies-total-budget-range     450\nset knowledge-investment-preference-mean 0.02\nset knowledge-investment-preference-range 0.02\nset companies-risk-appetite-std      0.005\nset SOE1-active?                     true\nset SOE1-sustainable?                true\nset SOE2-active?                     true\nset constant-externals?              true\nset experiments-demand?              false
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
15
446
223
479
SOE1-risk-appetite-increase
SOE1-risk-appetite-increase
0
0.10
0.03
0.001
1
NIL
HORIZONTAL

MONITOR
1209
566
1449
611
SOE1 cumulative knowledge investments [M€]
([cumulative-knowledge-investments] of SOE1) / 1E6
2
1
11

MONITOR
1454
517
1630
562
SOE1 cumulative asset profit [M€]
reporter-cumulative-profit-SOE1
2
1
11

MONITOR
1962
516
2170
561
green electricity consumption [PJ]
reporter-total-green-electricity-consumption
0
1
11

SLIDER
14
327
225
360
companies-risk-appetite-mean
companies-risk-appetite-mean
0.0225
0.0675
0.045
0.001
1
NIL
HORIZONTAL

SLIDER
17
42
225
75
knowledge-factor
knowledge-factor
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
17
82
224
115
learning-by-doing-factor
learning-by-doing-factor
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
14
165
224
198
companies-total-budget-mean
companies-total-budget-mean
0
1000
525.0
1
1
M€
HORIZONTAL

SLIDER
14
205
224
238
companies-total-budget-range
companies-total-budget-range
0
1000
450.0
0.1
1
M€
HORIZONTAL

SLIDER
14
244
225
277
knowledge-investment-preference-mean
knowledge-investment-preference-mean
0.00
0.1
0.02
0.001
1
NIL
HORIZONTAL

SLIDER
16
287
226
320
knowledge-investment-preference-range
knowledge-investment-preference-range
0.00
0.1
0.02
0.001
1
NIL
HORIZONTAL

SLIDER
14
367
224
400
companies-risk-appetite-std
companies-risk-appetite-std
0.0025
0.0075
0.005
0.0001
1
NIL
HORIZONTAL

SLIDER
14
407
224
440
companies-risk-increase
companies-risk-increase
0.005
0.015
0.01
0.001
1
NIL
HORIZONTAL

SWITCH
239
465
446
498
constant-externals?
constant-externals?
0
1
-1000

TEXTBOX
500
16
722
50
Demand curve parameters
11
0.0
1

TEXTBOX
903
16
1053
34
Interface
11
0.0
1

TEXTBOX
1723
16
1873
34
Output
11
0.0
1

MONITOR
1635
566
1803
611
total production capacity [PJ]
reporter-total-production-capacity
2
1
11

SWITCH
16
490
223
523
SOE1-active?
SOE1-active?
0
1
-1000

MONITOR
1806
566
1958
611
capacity factor [-]
reporter-capacity-factor
3
1
11

MONITOR
1807
516
1958
561
relative demand shortage [-]
reporter-relative-demand-shortage
3
1
11

SWITCH
240
687
449
720
experiments-demand?
experiments-demand?
1
1
-1000

SLIDER
239
763
449
796
slope-demand
slope-demand
0
0.5
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
241
724
449
757
fraction-max-demand
fraction-max-demand
0
1
1.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
303
663
453
681
Experimentation
11
0.0
1

TEXTBOX
1405
679
1555
697
Debugging
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This model gives a representation of the emergence of a hydrogen economy and allows for the exploration of the effects of state-owned enterprises (SOEs) on that emergence. The model has originally been developed as part of a master's thesis research project for the MSc Complex Systems Engineering and Management at Delft University of Technology. This master's thesis research project was carried out as an exploratory study for EBN B.V.

This info tab gives a short overview of the model. Detailed documentation of the model, its workings, and the underlying concepts, can be found in the thesis at https://repository.tudelft.nl/islandora/object/uuid%3Adc14e9df-0740-4078-bb2e-7a9df9a71c16?collection=education

This model has been adapted slightly from the original model used for the thesis, to make the model more generally applicable. The adaptation has been to rename the state-owned enterprises originally used in the thesis to 'SOE1' and 'SOE2'.

## HOW IT WORKS

The model contains three agents: hydrogen production companies, production assets and hydrogen demand sectors. State-owned enterprises are a subclass of the parent class 'companies'. Over time, the demand sectors develop a certain hydrogen demand. Every year companies then compete for demand contracts by realizing production assets with different hydrogen production technologies. These production assets then produce hydrogen and supply it to the demand sectors according to their contracts. The model runs from 2020 to 2050. Between runs, changes can be made to the ways that the state-owned enterprises are allowed to affect the development of the hydrogen economy.

A **model narrative** describing the procedures can be found in Chapter 6.2 of the thesis.

An **activity diagram** with an overview of the procedures can be found in Figure 6.2 of the thesis.

Detailed **flowcharts** for each procedure can be found in Appendix E of the thesis.

The go procedure has 5 general phases: preparation, planning, initiation, execution, and operation. These phases are maintatined throughout the model narrative, the activity diargram and in the model code.

## HOW TO USE IT

The model interface is divided into three parts: input, visualisation and output.

**Input**
On the left hand side, input parameters for the model run can be set before the model run. Base case values have been identified, which can be found in Appendix F of the thesis. The 'base case' button automatically sets all the input parameters to the base case values. If 'constant-externals?' is turned on, the external input parameters will be fixed at the input value during the model run. If 'constant-externals?' is turned off, the external input parameters will be set at the base case value for 2020 and linearly develop towards the input value by 2050. By changing the demand curve parameters de development of demand over time can be adjusted for each of the four demand sectors.

The levers form the most important part of the input, as the exploration of these levers is the goal to which the model has been created. By changing the levers the asset investment behaviour of SOE1 can be changed. A description of input levers can be found in Table 6.1 of the thesis.

It is important to note that there is a conceptual difference between SOE1 and SOE2.
SOE1 is the main state-owned enterprise in the model. A lot of its parameters are adjustable, including whether or not its investments are sustainable by default. It has no limits to its asset investment budget, instead, the cumulative investments of SOE1 are tracked through output monitors. SOE2 in contrast is very similar to a "regular" company. The only difference is that the asset investment behaviour of SOE2 is limited to 'non-mandatory-participation' by default.

**Visualisation**
In the middle of the interface the visual representation of the model run is shown. During model run, multiple companies can participate in the realization of a production asset. The 'hide-shares?' switch can either hide or visualize the shares that the participating companies have in a production asset.

The demand sectors are setup as central points of hydrogen demand. Production assets are realized relative to these demand sectors, at locations that correspond with the distance to each sector that they are contracted with.

**Output**
On the right hand side, output of the model is given during the model run in monitors and plots. A description of key output metrics can be found in Table 6.2 of the thesis.	


## EXTENDING THE MODEL

The following extensions to the model are suggested, in no particular order:

- **Multimodels of demand:** instead of representing the development of demand by predefined demand curves, each demand sector could be represented by an individual model. Since in this model demand developmend is externalized, it should be conceptually straightforward to couple more detailed demand models to this model. This model would then form the core model in a multimodel ensemble.

- **Timeseries for external input parameters:** the external input parameters for variables such as the electricity price, the natural gas price etc. indicate the end-state of that variable in 2050 if 'constant-externals?' is turned off. At model setup a linearly changing timeseries for the base case value in 2020 to the end-state in 2050 is generated for each of these variables. A similar approach is taken for the demand development in the four sectors, though these  follow a predefined curve. Instead of these predefined timeseries, detailed timeseries from external scenarios can be inserted and used as input in the model.

- **Changing production technologies:** every hydrogen production technology available to a production asset is defined as a list. All these technologies are combined in the list of lists 'technologies'. Technologies can be easily removed from and added to this list of lists in the setup procedure. An example would be to differentiate between SMR with and without CCS. Extending the model in this way would require an update of the setup procedure, as some input values are technology-specific. It would also require an update of the go procedure, as some procedures are based on a fixed number of available technologies.

- **Introducing storage technologies:** the asset investment procedure can be extended by allowing companies to investment in hydrogen storage assets. A storage asset investment procedure similar to the production asset investment procedure could then be implemented in the model. This would require to conceptually develop the role of storage in the hydrogen economy and the incentive for companies to invest in storage assets.

- **Construction time:** the model can be extended by introducing a variable construction time for hydrogen production assets. Some basis for this extension has already been made in the model. To further implement this it would be required to specify a construction time for each production technology in the setup procedure. Additionally, it would be required to conceptually develop the way in which companies perceive future demand in the 'companies-set-expected-demand' procedure.

## NETLOGO FEATURES

To spatially represent the distance between a production asset and demand sectors, the layout-spring procedure is applied. The turtle-set of the layout-spring procedure is the production asset. Because demand sectors are not part of the turtle-set but are still connected to the production asset through contracts, they are treated as the anchors and are not moved. The contract-distance that is specified for each contract that a production asset has, should then be used as a spring-length. Netlogo however does not allow heterogeneous springs in the layout-spring procedure. Therefore, as a workaround, the layout-spring procedure is run seperately for each contract in the 'set-location' procedure.

This workaround provides a suitable visual representation of the positioning of production assets relative to their contracted demand sectors, according to the distance specified in their contracts. Due to the lack of heterogeneous spring-lenghts, it however does not give an entirely accurate representation of the relative contract distances.

## CREDITS AND REFERENCES

Bijkerk, M.A. (2020). _The influence of state participation on the emergence of a hydrogen economy in the Netherlands_ (Master's thesis, Delft University of Technology, Delft, The Netherlands). Retrieved from https://repository.tudelft.nl/islandora/object/uuid%3Adc14e9df-0740-4078-bb2e-7a9df9a71c16?collection=education

This model has been experimented with using Exploratory Modelling and Analysis (EMA). More information on EMA can be found at https://emaworkbench.readthedocs.io/en/latest/

The development of this model has been supported by the Guide for Good Modelling Practice in policy support. This modelling guide can be found at https://pure.tudelft.nl/portal/files/55373868/gmp.pdf

Copyright (c) 2020 Mathijs Bijkerk

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

building institution
false
0
Rectangle -7500403 true true 0 60 300 270
Rectangle -16777216 true false 130 196 168 256
Rectangle -16777216 false false 0 255 300 270
Polygon -7500403 true true 0 60 150 15 300 60
Polygon -16777216 false false 0 60 150 15 300 60
Circle -1 true false 135 26 30
Circle -16777216 false false 135 25 30
Rectangle -16777216 false false 0 60 300 75
Rectangle -16777216 false false 218 75 255 90
Rectangle -16777216 false false 218 240 255 255
Rectangle -16777216 false false 224 90 249 240
Rectangle -16777216 false false 45 75 82 90
Rectangle -16777216 false false 45 240 82 255
Rectangle -16777216 false false 51 90 76 240
Rectangle -16777216 false false 90 240 127 255
Rectangle -16777216 false false 90 75 127 90
Rectangle -16777216 false false 96 90 121 240
Rectangle -16777216 false false 179 90 204 240
Rectangle -16777216 false false 173 75 210 90
Rectangle -16777216 false false 173 240 210 255
Rectangle -16777216 false false 269 90 294 240
Rectangle -16777216 false false 263 75 300 90
Rectangle -16777216 false false 263 240 300 255
Rectangle -16777216 false false 0 240 37 255
Rectangle -16777216 false false 6 90 31 240
Rectangle -16777216 false false 0 75 37 90
Line -16777216 false 112 260 184 260
Line -16777216 false 105 265 196 265

building store
false
0
Rectangle -7500403 true true 30 45 45 240
Rectangle -16777216 false false 30 45 45 165
Rectangle -7500403 true true 15 165 285 255
Rectangle -16777216 true false 120 195 180 255
Line -7500403 true 150 195 150 255
Rectangle -16777216 true false 30 180 105 240
Rectangle -16777216 true false 195 180 270 240
Line -16777216 false 0 165 300 165
Polygon -7500403 true true 0 165 45 135 60 90 240 90 255 135 300 165
Rectangle -7500403 true true 0 0 75 45
Rectangle -16777216 false false 0 0 75 45

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

electric outlet
false
0
Rectangle -7500403 true true 45 0 255 297
Polygon -16777216 false false 120 270 90 240 90 195 120 165 180 165 210 195 210 240 180 270
Rectangle -16777216 true false 169 199 177 236
Rectangle -16777216 true false 169 64 177 101
Polygon -16777216 false false 120 30 90 60 90 105 120 135 180 135 210 105 210 60 180 30
Rectangle -16777216 true false 123 64 131 101
Rectangle -16777216 true false 123 199 131 236
Rectangle -16777216 false false 45 0 255 296

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

factory ch4
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270
Line -7500403 true 165 105 165 165
Line -7500403 true 165 135 195 135
Line -7500403 true 225 135 225 150
Line -7500403 true 225 150 240 150
Line -7500403 true 105 165 135 165
Line -7500403 true 105 105 135 105
Line -7500403 true 105 105 105 165
Line -7500403 true 240 135 240 165
Line -7500403 true 195 105 195 165

factory h2
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270
Line -7500403 true 135 105 135 165
Line -7500403 true 135 135 165 135
Line -7500403 true 165 105 165 165
Line -7500403 true 195 135 210 135
Line -7500403 true 210 135 210 150
Line -7500403 true 195 150 195 165
Line -7500403 true 195 150 210 150
Line -7500403 true 195 165 210 165

factory-ch4
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270
Polygon -1 true false 240 90 240 135 270 135 270 165 285 165 285 90 270 90 270 120 255 120 255 90 240 90
Polygon -1 true false 195 90 180 90 180 165 195 165 195 135 210 135 210 165 225 165 225 90 210 90 210 120 195 120
Polygon -1 true false 165 120 165 105 150 90 135 90 120 105 120 150 135 165 150 165 165 150 165 135 150 135 150 150 135 150 135 105 150 105 150 120

factory-h2
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270
Polygon -1 true false 195 90 180 90 180 165 195 165 195 135 210 135 210 165 225 165 225 90 210 90 210 120 195 120
Polygon -1 true false 240 120 240 105 255 90 270 90 285 105 285 120 255 150 285 150 285 165 240 165 240 150 270 120 270 105 255 105 255 120

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

lightning
false
0
Polygon -7500403 true true 120 135 90 195 135 195 105 300 225 165 180 165 210 105 165 105 195 0 75 135

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

orbit 1
true
0
Circle -7500403 true true 116 11 67
Circle -7500403 false true 41 41 218

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

trucks
0.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
