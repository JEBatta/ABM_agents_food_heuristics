globals[
;; parameters

  ;; consumat sims params
  ;; dummy variables
  mylist
  patch-reg-dev
  ii
  treg
  ;;outputs
  pop
  pop1
  pop2
  pop3
  pop4
  pop5
  pop6
  pop7
  pop8
  pop9
  popA
  popB
  pop-rep
  pop-imit
  pop-inq
  pop-opt
  global-sat-e;CONSUMAT outputs start here ;satisfaction
  global-sat-p
  global-sat-s
  rate-satisfaction
  global-uncertainty ; CONSUMAT uncertainty
  rate-uncertainty
  av-energy
  e1
  e2
  e3
  e4
  e5
  e6
  e7
  e8
  e9
  e-rep
  e-imit
  e-inq
  e-opt
  pop-acc-a
  pop-acc-b
  en-acc-a
  en-acc-b
  time-abundance-t
  time-famine-t
  season-accum
  season-t
  season-time
  number-cycles

  rate-time-richer
]
breed [persons person]
persons-own [
  energy
  energy-max
  dE
  dE-estimate
  dE-variance
  accumulation?
  acc-limit
  limit-dif
  agent-eating-fraction
  strategy
  strategy-type-food
  increment
  eat?
  eat-before?
  move?
  ;; CONSUMAT variables
  sat-e ;; satisfaction
  sat-p
  sat-s
  threshold-sat
  max-sat-e
  min-sat-e
  min-energy-social
  max-sat-s
  min-sat-s
  n-alpha
  n-action
  uncertainty ;;
  threshold-u
  max-u
  min-u
  rule
  satisfied?
  certainty?
  utilities-p
  utilities-e
  utilities-s
  estimate-food-neighbors
  pool-of-strategies
  history-rule
  history-sat
  history-u
  time-richer
  ]

patches-own[
  source-type
  source-energy
  Es
  food-here?
  xoasis?
  tag
  xtreg
  xpgrow
]

 to setup
  clear-all
  set-default-shape persons "person"
  set-for-sims

  ;;; Only for simulations AvsB
  set acc-limit-a acc-limit-global
  set acc-limit-b acc-limit-global
  set initial-strategy-a initial-strategy-global
  set initial-strategy-b initial-strategy-global
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  create-persons (pop-total) [
    set eat? false
    set eat-before? false
    set move? false
    set accumulation? true
    set-initial-strategy-and-rule
    move-to one-of patches with [not any? persons-here]
    set energy E0
    set energy-max energy
    set dE 0
    set dE-estimate 0
    set dE-variance 0
    set max-sat-e 0
    set min-sat-e 0
    set min-energy-social energy
    set max-sat-s 0
    set min-sat-s 0
    set n-alpha 1
    set n-action 1
    set uncertainty 0
    set max-u uncertainty
    set min-u uncertainty
    set agent-eating-fraction eating-fraction
    set threshold-sat random-float 0.1
    set threshold-u random-float 0.1
    set satisfied? true
    set certainty? true
    set utilities-p [0 0 0 0 0 0 0 0 0]
    set utilities-e [0 0 0 0 0 0 0 0 0]
    set utilities-s [0 0 0 0 0 0 0 0 0]
    set history-rule ""
    set history-sat ""
    set history-u ""
    set time-richer 0
  ]

  ask persons[
    set-pool-of-strategies
  ]

  ask patches [
    ifelse random-float 1 <= rate-richer-food [set source-type "richer"][set source-type "normal"]

    if isles-of-richer-food [
      ifelse pxcor <= 14 and pxcor >= -14 and pycor <= 14 and pycor >= -14 [set source-type "richer"][set source-type "normal"]
    ]
    ifelse source-type = "richer" [set Es Es-richer-food][set Es Es-normal-food]
    set tag 0
    set xtreg treg

    ;; escribir aqui codigo para establecer regiones completas y juntas
    ;; de parches con comida energetica ( esto servira para ver si los agentes
    ;; se desplazan hacia estas regiones y si después se mantienen ahi)

    if sources-distribution = "random-on-time" or sources-distribution = "constant" [set xpgrow p-grow]
    if sources-distribution = "random-on-space" or sources-distribution = "random-on-space-and-time" [set xpgrow random-float 1.0]

    set pcolor white
    set food-here? false
    ifelse random-float 1 <= p-oasis [set xoasis? true][set xoasis? false]
    if regeneration-type = "oasis" [
      ifelse xoasis? [set xpgrow p-grow][set xpgrow 0]
    ]
    if regeneration-type = "vacancies" [
      ifelse xoasis? [set xpgrow 1][set xpgrow p-grow]
    ]
    if regeneration-type = "deterministic" [
      set food-here? true
      ifelse source-type = "richer" [set pcolor green][set pcolor yellow]
      ifelse source-type = "richer" [set Es Es-richer-food][set Es Es-normal-food]
      set tag 0
    ]
  ]
  if famine-abundance [setup-abundance-famine]
  reset-ticks
end

to go
  if not any? persons [ stop ]
  if famine-abundance [go-abundance-famine]
  grow-sources
  ask persons
  [
    perceive
    if movement-type = "simple" [move-simple]
    if movement-type = "composed" [move-composed]
    eat
    update-energy
    update-strategy
    update-satisfaction
    update-uncertainty
    if decision-rule-mode = "dynamic" [update-rule]
    death
  ]
  if history-record [write-file-history]
  ask patches with [source-type = "richer"][
    ask persons-here [set time-richer time-richer + 1]
  ]
  if any? persons [set-globals]
  tick
end

to setup-abundance-famine

ifelse season-stochastic [
      set time-famine-t floor random-normal time-famine std-season
      set time-abundance-t floor random-normal time-abundance std-season
      set season-t time-famine-t + time-abundance-t
      set season-accum 1
    ]
    [
      set time-famine-t time-famine
      set time-abundance-t time-abundance
     ]
    set number-cycles 0
    ask patches [set xpgrow p-grow-abundance]

end

to go-abundance-famine
  if season-stochastic [
  set season-time ticks - season-accum
  if season-stochastic and ticks > 0 and season-time = season-t [
    set season-accum season-accum + season-t
    set time-famine-t floor random-normal time-famine std-season
    set time-abundance-t floor random-normal time-abundance std-season
    set season-t time-famine-t + time-abundance-t
    set number-cycles number-cycles + 1
  ]
  ]
  set-famine-abundance
end

to set-initial-strategy-and-rule

  if tournament = "accumulation" or tournament = "self-rationing-accumulation" [
    set strategy initial-strategy-a
    set strategy-type-food one-of initial-food-afinity-a
    if who mod 2 = 0 [set accumulation? false set color red]
    if who mod 2 = 1 [set accumulation? true set color violet]
  ]
  if tournament = "binary" or tournament = "accumulation-limit-binary" [
    if who mod 2 = 0 [set strategy initial-strategy-a
                      set strategy-type-food initial-food-afinity-a
                      set acc-limit acc-limit-a
                      set color red]
    if who mod 2 = 1 [set strategy initial-strategy-b
                      set strategy-type-food initial-food-afinity-b
                      set acc-limit acc-limit-b
                      set color violet]
  ]
  if tournament = "none" [
    set strategy one-of ["S1" "S2" "S3" "S4" "S5" "S6" "S7" "S8" "S9"]
    set strategy-type-food one-of ["A" "B"]
    set color black
  ]
  if tournament = "all" [
    if who mod 9 = 0 [set strategy "S1" set color 1]
    if who mod 9 = 1 [set strategy "S2" set color 2]
    if who mod 9 = 2 [set strategy "S3" set color 3]
    if who mod 9 = 3 [set strategy "S4" set color 4]
    if who mod 9 = 4 [set strategy "S5" set color 5]
    if who mod 9 = 5 [set strategy "S6" set color 6]
    if who mod 9 = 6 [set strategy "S7" set color 7]
    if who mod 9 = 7 [set strategy "S8" set color 8]
    if who mod 9 = 8 [set strategy "S9" set color 9]
    set strategy-type-food one-of ["A" "B"]
  ]
  if decision-rule-mode = "one-rule" [
    set rule one-rule
  ]
  if decision-rule-mode = "dynamic" [
    if who mod 4 = 0 [set rule "repetition"]
    if who mod 4 = 1 [set rule "imitation" ]
    if who mod 4 = 2 [set rule "inquiring" ]
    if who mod 4 = 3 [set rule "optimizing"]
  ]
end

to grow-sources
  ask patches with [not food-here?] [
    if regeneration-type = "oasis" [
    ifelse xoasis? and random-float 1 < xpgrow [
      set food-here? true
      ifelse source-type = "richer" [set pcolor green][set pcolor yellow]
      set source-energy Es
      set tag 0
    ][
      set food-here? false
      set pcolor white
      set source-energy 0
      set tag 0
    ]
    ]
    if regeneration-type = "vacancies" [
    ifelse not xoasis? and random-float 1 > xpgrow [
      set food-here? false
      set pcolor white
      set source-energy 0
      set tag 0
    ][
      set food-here? true
      ifelse source-type = "richer" [set pcolor green][set pcolor yellow]
      set source-energy Es
      set tag 0
    ]
    ]
    if regeneration-type = "deterministic" and tag = treg [
      set food-here? true
      ifelse source-type = "richer" [set pcolor green][set pcolor yellow]
      set source-energy Es
      set tag 0
    ]
    if regeneration-type = "all-patches" [
      ifelse random-float 1 < xpgrow [
      set food-here? true
      ifelse source-type = "richer" [set pcolor green][set pcolor yellow]
      set source-energy Es
      set tag 0
    ][
      set food-here? false
      set pcolor white
      set source-energy 0
      set tag 0
    ]
    ]
  ]
end

to eat
  let eat-energy 0
  if strategy = "S2" or strategy = "S4" or strategy = "S6" or strategy = "S8" or strategy = "S9" [
    ask patch-here [
    if food-here? [
      ifelse (Es * [agent-eating-fraction] of myself) >= source-energy [
        set eat-energy source-energy
        set source-energy 0
        set food-here? false
        set pcolor white
        set tag 0
      ][
        set source-energy source-energy - Es * [agent-eating-fraction] of myself
        set eat-energy Es * [agent-eating-fraction] of myself
      ]
  ]
  ]
  ]
  set increment eat-energy
  set eat-before? eat?
  ifelse increment = 0 [set eat? false][set eat? true]
end
to perceive
  if strategy = "S3" or strategy = "S4" or strategy = "S7" or strategy = "S8" or strategy = "S9" [
    ifelse (count neighbors with [food-here?]) > 0
    [set estimate-food-neighbors 1]
    [set estimate-food-neighbors 0]
  ]
  if strategy = "S1" or strategy = "S2" or strategy = "S5" or strategy = "S6" [set estimate-food-neighbors 0.5]
end
to move-simple
  set move? false
  if strategy = "S6" or strategy = "S5" [move-to one-of neighbors set move? true]
  if  strategy = "S7" or strategy = "S8" [
    ifelse estimate-food-neighbors = 1 [
      ifelse (strategy-type-food = "B" and any? neighbors with [food-here? and source-type = "richer"])[
      move-to one-of neighbors with [food-here? and source-type = "richer"]
      ][
      move-to one-of neighbors with [food-here?]
      ]
    ][
      move-to one-of neighbors
      ]

    set move? true
  ]


  if strategy = "S9" [
    if not [food-here?] of patch-here and estimate-food-neighbors = 1
    [
      ifelse (strategy-type-food = "B" and any? neighbors with [food-here? and source-type = "richer"])[
      move-to one-of neighbors with [food-here? and source-type = "richer"]
      ][
      move-to one-of neighbors with [food-here?]
      ]
      set move? true]
    ]
end
to move-composed
  let p 1
  ifelse (energy > 2 and energy < Emax )[move-simple][
    if (energy < 2)[set p exp(energy - 2)]
    if (energy > Emax) [set p exp(Emax - energy)]
    ifelse random-float 1 <= p [move-simple][set move? false]
  ]
end

to update-energy
  let a0 0
  let a1 0
  if metabolism-type = "lineal" [set a1  (- Mb)]
  if metabolism-type = "constant" [set a0 (- Mb * Emax)]
  if metabolism-type = "constant-lineal" [
    ifelse energy < Emax
    [set a0 (- Mb * Emax)]
    [set a1 (- Mb)]]
  if metabolism-type = "accumulation-punishment" [
   set a0 (-1 * Mb * energy-max)
  ]
  if strategy = "S3" or strategy = "S4" or strategy = "S7" or strategy = "S8" or strategy = "S9"
  [set a0 a0 - CostP]
  if strategy-type-food = "B"
  [set a0 a0 - CostTypeB]
  if move? [set a1 a1 - Cm]
  if tournament = "accumulation"[
  ifelse accumulation?
  [
    set dE a0 + increment + a1 * energy
    set energy energy + dE
    set increment 0
  ]
  [
    set dE a0 + increment + a1 * energy - energy
    set energy a0 + increment + a1 * energy
    set increment 0
    ]
  ]

  if tournament = "accumulation-limit-binary"[
    set dE a0 + increment + a1 * energy
    ifelse (energy + dE) > acc-limit [
      set limit-dif (energy + dE - acc-limit)
      set energy acc-limit
    ] [
      set limit-dif (- energy - dE + acc-limit)
      set energy energy + dE
    ]
    set increment 0
  ]
    if tournament = "self-rationing-accumulation"[
  ifelse accumulation?
  [
    set dE a0 + increment + a1 * energy
    set energy energy + dE
    set increment 0
  ]
  [
   let temp a0 + increment + a1 * energy - 1
   if temp > 0 [ask patch-here [set source-energy temp] ]
   set dE 1 - energy
   set energy 1
   set increment 0
  ]
  ]
  if tournament = "binary" or tournament = "none" or tournament = "all" [
    set dE a0 + increment + a1 * energy
    set energy energy + dE
    set increment 0
  ]
  ;set dE-estimate (dE-estimate * (ticks) + dE)/ (ticks + 1)
  set dE-estimate dE
  ;set dE-variance (dE-variance * (ticks) + (dE - dE-estimate) ^ 2 )/ (ticks + 1)
  set dE-variance 0
  if energy > energy-max [set energy-max energy]
end

to update-satisfaction
  ;; existential need satisfacation
  let t-sat-e dE
  if t-sat-e > max-sat-e [set max-sat-e t-sat-e]
  if t-sat-e < min-sat-e [set min-sat-e t-sat-e]
  if (max-sat-e - min-sat-e) != 0 [set sat-e (t-sat-e - min-sat-e)/(max-sat-e - min-sat-e)]
  ;; other needs
  ;; personality need satisfaction
  if eat? and eat-before? [set sat-p 1.0]
  if eat? and not eat-before? [set sat-p 0.83333]
  if not eat? and eat-before? [set sat-p 0.166667]
  if not eat? and not eat-before? [set sat-p 0.0]
  ;; social need satisfaction
  let t-sat-s -1
  if strategy = "S1" or strategy = "S2" or strategy = "S5" or strategy = "S6" [
    if min [energy] of turtles-here < min-energy-social [set min-energy-social min [energy] of turtles-here]
    if count turtles-here > 1 [
      set t-sat-s -1 * abs (min-energy-social - energy)
    ]
  ]
  if strategy = "S3" or strategy = "S4" or strategy = "S7" or strategy = "S8" or strategy = "S9" [
    let temp min [energy] of turtles-on (patch-set patch-here neighbors)
    if temp < min-energy-social [set min-energy-social temp]
    if count turtles-on (patch-set patch-here neighbors) > 1 [
      set t-sat-s -1 * abs (min-energy-social - energy)
    ]
  ]
  if t-sat-s > max-sat-s [set max-sat-s t-sat-s]
  if t-sat-s < min-sat-s [set min-sat-s t-sat-s]
  if (max-sat-s - min-sat-s) != 0 [set sat-s (t-sat-s - min-sat-s)/(max-sat-s - min-sat-s)]

  ;; stablish satisfaction status
  if needs = "complete-set" [
  ifelse sat-s >= threshold-sat and sat-e >= threshold-sat and sat-p >= threshold-sat
  [set satisfied? true][set satisfied? false]
  ]
  if needs = "only-existential" [
  ifelse sat-e >= threshold-sat
    [set satisfied? true][set satisfied? false]
  ]
end
to update-uncertainty
  if strategy = "S1" or strategy = "S2" or strategy = "S5" or strategy = "S6" [
    ;set n-alpha n-alpha - 1 + count turtles-here
    set n-alpha count turtles-here
    ;set n-action n-action - 1 + count turtles-here with [strategy = [strategy] of myself]
    set n-action count turtles-here with [strategy = [strategy] of myself]
  ]
  if strategy = "S3" or strategy = "S4" or strategy = "S7" or strategy = "S8" or strategy = "S9" [
    set n-alpha n-alpha - 1 + count turtles-on (patch-set patch-here neighbors)
    set n-action n-action - 1 + count (turtles-on (patch-set patch-here neighbors)) with [strategy = [strategy] of myself]
  ]
  let t-u 0.5 * dE-variance + 0.5 * (n-alpha - n-action) / n-alpha
  if t-u > max-u [set max-u t-u]
  if t-u < min-u [set min-u t-u]
  if (max-u - min-u) != 0 [set uncertainty (t-u - min-u)/(max-u - min-u)]
  ;; stablish uncertainty status
  ifelse uncertainty >= threshold-u
  [set certainty? false][set certainty? true]
end
to update-rule
  if satisfied? and certainty?  [set rule "repetition"]
  if satisfied? and not certainty?  [set rule "imitation"]
  if not satisfied? and certainty?  [set rule "optimizing"]
  if not satisfied? and not certainty?  [set rule "inquiring"]
end

to calculate-utilities
    ;; diference in utilities of personal needs
    if eat? and move? and estimate-food-neighbors = 0.5 [set utilities-p [-1 -0.5 -1 -0.5 -1 0 -1 0 0]]
    if eat? and move? and estimate-food-neighbors = 1 [set utilities-p [-1 -0.5 -1 -0.5 -1 -0.5 -1 0 0]]
    if eat? and move? and estimate-food-neighbors = 0 [set utilities-p [-1 -0.5 -1 -0.5 -1 -0.5 -1 0 -0.5]]
    if eat? and not move? and estimate-food-neighbors = 0.5 [set utilities-p [-1 0 -1 0 -1 -0.5 -1 -0.5 0]]
    if eat? and not move? and estimate-food-neighbors = 1 [set utilities-p [-1 0 -1 0 -1 -0.5 -1 0 0]]
    if eat? and not move? and estimate-food-neighbors = 0 [set utilities-p [-1 0 -1 0 -1 0 -1 0 0]]
    if not eat? and move? and estimate-food-neighbors = 0.5 [set utilities-p [0 0.5 0 0.5 0 0 0 0 0.5]]
    if not eat? and move? and estimate-food-neighbors = 1 [set utilities-p [0 0.5 0 0.5 0 0 0 0 0.5]]
    if not eat? and move? and estimate-food-neighbors = 0 [set utilities-p [0 0.5 0 0.5 0 0.5 0 0 0]]
    if not eat? and not move? and estimate-food-neighbors = 0.5 [set utilities-p [0 0 0 0 0 0.5 0 0.5 0.5]]
    if not eat? and not move? and estimate-food-neighbors = 1 [set utilities-p [0 0 0 0 0 1 0 1 0.5]]
    if not eat? and not move? and estimate-food-neighbors = 0 [set utilities-p [0 0 0 0 0 0 0 0 0]]

    if strategy = "S9" and move? [
      ifelse eat?
      [set utilities-p replace-item 1 utilities-p -1
       set utilities-p replace-item 3 utilities-p -1]
      [set utilities-p replace-item 1 utilities-p 0
       set utilities-p replace-item 3 utilities-p 0]
    ]
    ;; utilities of existential needs
    let c1 CostP
    if strategy-type-food = "B" [set c1 c1 + CostTypeB]
    let c2 Cm * energy
    if strategy = "S1" or strategy = "S2" [set utilities-e (list 0 0 (- c1) (- c1) (- c2) (- c2) (- c1 - c2) (- c1 - c2) (- c1 - c2 / 2))]
    if (strategy = "S3" or strategy = "S4")[
      set utilities-e ( list c1 c1 0 0 ( c1 - c2) ( c1 - c2) ( - c2) ( - c2) (- c2))
      if estimate-food-neighbors = 0 [set utilities-e replace-item 8 utilities-e 0]
    ]
    if strategy = "S5" or strategy = "S6" [set utilities-e (list c2 c2 ( c2 - c1) ( c2 - c1) 0 0 (- c1 ) (- c1 ) (- c1 + c2 / 2))]
    if (strategy = "S7" or strategy = "S8")[
      set utilities-e ( list ( c1 + c2 ) ( c1 + c2 )  c2 c2 c1 c1 0 0 0)
      if estimate-food-neighbors = 0 [set utilities-e replace-item 8 utilities-e c2]
    ]
    if strategy = "S9"[
      if estimate-food-neighbors = 1 [set utilities-e (list ( c1 + c2 ) ( c1 + c2 ) c2 c2 c1 c1 0 0 0)]
      if estimate-food-neighbors = 0 [set utilities-e (list c1 c1 0 0 ( c1 - c2 ) ( c1 - c2) (- c2) (- c2) 0)]
    ]
    set utilities-e (map + (map [i -> i * Es] utilities-p) utilities-e)
    ;; utilities of social needs
    set utilities-s map [i -> abs(min-sat-e - energy - i) - abs(min-sat-e - energy)] utilities-e
    ;; normalization of utilities
    set utilities-p map [i -> (i + 1) / 2 ] utilities-p
    set utilities-e map [i -> (i - c1 - c2 - Es) / 2 * (c1 + c2 + Es) ] utilities-e
    set utilities-s map [i -> (i - c1 - c2 - Es) / 2 * (c1 + c2 + Es) ] utilities-s
end

to update-strategy
  if type-of-strategies = "actions" [
  if rule = "optimizing" or rule = "inquiring" [
    calculate-utilities
  ]
  if rule = "optimizing" [
    let tt max (map + utilities-p utilities-e utilities-s)
    let tidx position tt (map + utilities-p utilities-e utilities-s)
    if needs = "only-existential" [
      set tt max utilities-e
      set tidx position tt utilities-e
    ]

    let prev-s strategy
    if tidx = 0 [set strategy "S1"]
    if tidx = 1 [set strategy "S2"]
    if tidx = 2 [set strategy "S3"]
    if tidx = 3 [set strategy "S4"]
    if tidx = 4 [set strategy "S5"]
    if tidx = 5 [set strategy "S6"]
    if tidx = 6 [set strategy "S7"]
    if tidx = 7 [set strategy "S8"]
    if tidx = 8 [set strategy "S9"]
  ]
  if rule = "inquiring" or rule = "imitation" [
    set-pool-of-strategies
  ]
  if rule = "inquiring"[

    let idx-list []
    if member? "S1" pool-of-strategies [set idx-list fput 0 idx-list]
    if member? "S2" pool-of-strategies [set idx-list fput 1 idx-list]
    if member? "S3" pool-of-strategies [set idx-list fput 2 idx-list]
    if member? "S4" pool-of-strategies [set idx-list fput 3 idx-list]
    if member? "S5" pool-of-strategies [set idx-list fput 4 idx-list]
    if member? "S6" pool-of-strategies [set idx-list fput 5 idx-list]
    if member? "S7" pool-of-strategies [set idx-list fput 6 idx-list]
    if member? "S8" pool-of-strategies [set idx-list fput 7 idx-list]
    if member? "S9" pool-of-strategies [set idx-list fput 8 idx-list]
    set idx-list remove-duplicates idx-list

    let idx-utilities map [i -> item i (map + utilities-p utilities-e utilities-s)] idx-list

    if needs = "only-existential" [
        set idx-utilities map [i -> item i utilities-e] idx-list
    ]

    let tt max idx-utilities
    let tidx item (position tt idx-utilities) idx-list

    if tidx = 0 [set strategy "S1"]
    if tidx = 1 [set strategy "S2"]
    if tidx = 2 [set strategy "S3"]
    if tidx = 3 [set strategy "S4"]
    if tidx = 4 [set strategy "S5"]
    if tidx = 5 [set strategy "S6"]
    if tidx = 6 [set strategy "S7"]
    if tidx = 7 [set strategy "S8"]
    if tidx = 8 [set strategy "S9"]

    ;if prev-s != strategy [
      ;set n-alpha 1
      ;set n-action 1
      ;set dE-estimate 0
      ;set dE-variance 0]
  ]
  if rule = "imitation" [
    let prev-s strategy
    set strategy one-of pool-of-strategies
    ;if prev-s != strategy [
      ;set n-alpha 1
      ;set n-action 1
      ;set dE-estimate 0
      ;set dE-variance 0]
  ]
  if tournament = "all" [set-color-person]
  ]
  if type-of-strategies = "accumulation" [
  ifelse rule = "imitation" or rule = "inquiring" [
    set-pool-of-strategies
    set acc-limit one-of pool-of-strategies
    ifelse acc-limit = acc-limit-a [set color red][set color violet]
  ] [
  if rule = "optimizing" [
   set acc-limit acc-limit-b
   set color violet
  ]
  ]
  ]
  if type-of-strategies = "food-afinity" [
    ifelse rule = "imitation" or rule = "inquiring" [
    set-pool-of-strategies
    set strategy-type-food one-of pool-of-strategies
    ifelse strategy-type-food = "A" [set color red][set color violet]
  ] [
  if rule = "optimizing" [
   set strategy-type-food "B"
   set color violet
  ]
  ]
  ]
end

to update-history
  if rule = "imitation" [set history-rule (word "I" history-rule) ]
  if rule = "repetition" [set history-rule (word "R" history-rule) ]
  if rule = "inquiring" [set history-rule (word "N" history-rule) ]
  if rule = "optimizing" [set history-rule (word "O" history-rule) ]
  ifelse satisfied? [set history-sat (word "S" history-sat)][set history-sat (word "U" history-sat)]
  ifelse certainty? [set history-u (word "C" history-u)][set history-u (word "U" history-u)]
end

to set-color-person
 if strategy = "S1" [set color black]
    if strategy = "S2" [set color gray]
    if strategy = "S3" [set color red]
    if strategy = "S4" [set color orange]
    if strategy = "S5" [set color brown]
    if strategy = "S6" [set color yellow]
    if strategy = "S7" [set color cyan]
    if strategy = "S8" [set color green]
    if strategy = "S9" [set color blue]
end

to set-pool-of-strategies
  if type-of-strategies = "actions" [
  if member? strategy  ["S1" "S2" "S5" "S6"] [
       set pool-of-strategies remove 0 ([strategy] of turtles-here)]
  if member? strategy  ["S3" "S4" "S7" "S8" "S9"] [
      set pool-of-strategies remove 0 ([strategy] of turtles-on (patch-set patch-here neighbors))]
  ]
  if type-of-strategies = "accumulation" [
  if rule = "imitation" [
    if member? strategy  ["S1" "S2" "S5" "S6"] [
       set pool-of-strategies remove 0 ([acc-limit] of turtles-here)]
    if member? strategy  ["S3" "S4" "S7" "S8" "S9"] [
      set pool-of-strategies remove 0 ([acc-limit] of turtles-on (patch-set patch-here neighbors))]
  ]
  if rule = "inquiring" [
    if member? strategy  ["S1" "S2" "S5" "S6"] [
      set pool-of-strategies remove 0 ([acc-limit] of (turtles-here with-max [energy]))]
    if member? strategy  ["S3" "S4" "S7" "S8" "S9"] [
      set pool-of-strategies remove 0 ([acc-limit] of ((turtles-on (patch-set patch-here neighbors)) with-max [energy]))
    ]
  ]
  ]
  if type-of-strategies = "food-afinity" [
  if rule = "imitation" [
    if member? strategy  ["S1" "S2" "S5" "S6"] [
       set pool-of-strategies remove 0 ([strategy-type-food] of turtles-here)]
    if member? strategy  ["S3" "S4" "S7" "S8" "S9"] [
      set pool-of-strategies remove 0 ([strategy-type-food] of turtles-on (patch-set patch-here neighbors))]
  ]
  if rule = "inquiring" [
    if member? strategy  ["S1" "S2" "S5" "S6"] [
      set pool-of-strategies remove 0 ([strategy-type-food] of (turtles-here with-max [energy]))]
    if member? strategy  ["S3" "S4" "S7" "S8" "S9"] [
      set pool-of-strategies remove 0 ([strategy-type-food] of ((turtles-on (patch-set patch-here neighbors)) with-max [energy]))
    ]
  ]
  ]
end
to set-famine-abundance
   ifelse number-cycles < final-cycle or not final-abundance-stage [
   ifelse season-time <= time-abundance-t [
      ask patches [set xpgrow p-grow-abundance]
     ][
      ask patches [set xpgrow p-grow-famine]
     ]
  ][
    ask patches [set xpgrow p-grow-abundance]
  ]
end

to reproduce
  if energy > Emax [
    set energy energy / 2
    ask patch-here [sprout-persons 1]
  ]
end
to-report calculate-hypothetical-satisfaction [hypothetical-strategy]

  report 0
end

to death     ;; turtle procedure: die if you run out of energy
  if energy <= 0 [ die ]
end

to set-globals ;; observer procedure
  ask patches with [not food-here?][set tag tag + 1]
  ;; CONSUMAT outputs
  set global-sat-e mean [sat-e] of persons
  set global-sat-p mean [sat-p] of persons
  set global-sat-s mean [sat-s] of persons
  set rate-satisfaction count persons with [satisfied?] / count persons
  set global-uncertainty mean [uncertainty] of persons
  set rate-uncertainty count persons with [not certainty?] / count persons
  ;; population outputs
  set pop count persons
  set pop1 count persons with [strategy = "S1"] / pop
  set pop2 count persons with [strategy = "S2"] / pop
  set pop3 count persons with [strategy = "S3"] / pop
  set pop4 count persons with [strategy = "S4"] / pop
  set pop5 count persons with [strategy = "S5"] / pop
  set pop6 count persons with [strategy = "S6"] / pop
  set pop7 count persons with [strategy = "S7"] / pop
  set pop8 count persons with [strategy = "S8"] / pop
  set pop9 count persons with [strategy = "S9"] / pop
  set popA count persons with [strategy-type-food = "A"] / pop
  set popB count persons with [strategy-type-food = "B"] / pop

  set pop-rep count persons with [rule = "repetition"] / pop
  set pop-imit count persons with [rule = "imitation"] / pop
  set pop-inq count persons with [rule = "inquiring"] / pop
  set pop-opt count persons with [rule = "optimizing"] / pop
  ;; energy
  set av-energy mean [energy] of persons
  set e1 ifelse-value any? persons with [strategy = "S1"][mean [energy] of persons with [strategy = "S1"]][0]
  set e2 ifelse-value any? persons with [strategy = "S2"][mean [energy] of persons with [strategy = "S2"]][0]
  set e3 ifelse-value any? persons with [strategy = "S3"][mean [energy] of persons with [strategy = "S3"]][0]
  set e4 ifelse-value any? persons with [strategy = "S4"][mean [energy] of persons with [strategy = "S4"]][0]
  set e5 ifelse-value any? persons with [strategy = "S5"][mean [energy] of persons with [strategy = "S5"]][0]
  set e6 ifelse-value any? persons with [strategy = "S6"][mean [energy] of persons with [strategy = "S6"]][0]
  set e7 ifelse-value any? persons with [strategy = "S7"][mean [energy] of persons with [strategy = "S7"]][0]
  set e8 ifelse-value any? persons with [strategy = "S8"][mean [energy] of persons with [strategy = "S8"]][0]
  set e9 ifelse-value any? persons with [strategy = "S9"][mean [energy] of persons with [strategy = "S9"]][0]
  set e-rep ifelse-value any? persons with [rule = "repetition"][mean [energy] of persons with [rule = "repetition"]][0]
  set e-imit ifelse-value any? persons with [rule = "imitation"][mean [energy] of persons with [rule = "imitation"]][0]
  set e-inq ifelse-value any? persons with [rule = "inquiring"][mean [energy] of persons with [rule = "inquiring"]][0]
  set e-opt ifelse-value any? persons with [rule = "optimizing"][mean [energy] of persons with [rule = "optimizing"]][0]

  ;; accumulation tournament

  set pop-acc-b count persons with [acc-limit = acc-limit-b]
  set pop-acc-a count persons with [acc-limit = acc-limit-a]
  set en-acc-b ifelse-value any? persons with [acc-limit = acc-limit-b][mean [energy] of persons with [acc-limit = acc-limit-b]][0]
  set en-acc-a ifelse-value any? persons with [acc-limit = acc-limit-a][mean [energy] of persons with [acc-limit = acc-limit-a]][0]

  ;;
  set rate-time-richer sum([time-richer] of persons)


end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; for simulations
to set-for-sims
  set treg round(1.0 / p-grow)
end

to write-file-history
    ask persons [
      file-open "history_register.csv"
      ;file-print (word p-grow "," ticks "," self "," strategy "," history-rule "," history-sat "," history-u)
      file-print (word p-grow "," ticks "," self "," strategy ","pool-of-strategies","energy)
      file-close
    ]
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
