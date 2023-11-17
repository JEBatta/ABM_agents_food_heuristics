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
  ]

patches-own[
  source-energy
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
  ]

  ask persons[
    ifelse tournament = "accumulation" or tournament = "accumulation-limit-binary"
    [set-pool-of-strategies-accumulation]
    [set-pool-of-strategies]
  ]

  ask patches [
    set source-energy Es
    set tag 0
    set xtreg treg

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
      set pcolor green
      set source-energy Es
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

    ifelse tournament = "accumulation" or tournament = "accumulation-limit-binary"
    [update-strategy-accumulation]
    [update-strategy]

    update-satisfaction
    update-uncertainty
    if decision-rule-mode = "dynamic" [update-rule]
    death
  ]
  if history-record [write-file-history]
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
    if who mod 2 = 0 [set accumulation? false set color red]
    if who mod 2 = 1 [set accumulation? true set color violet]
  ]
  if tournament = "accumulation-limit-binary" [
    set strategy initial-strategy-a
    set accumulation? true
    if who mod 2 = 0 [set acc-limit acc-limit-a set color red]
    if who mod 2 = 1 [set acc-limit acc-limit-b set color violet]
  ]
  if tournament = "binary" [
    if who mod 2 = 0 [set strategy initial-strategy-a set color red]
    if who mod 2 = 1 [set strategy initial-strategy-b set color violet]
  ]
  if tournament = "none" [
    set strategy one-of ["S1" "S2" "S3" "S4" "S5" "S6" "S7" "S8" "S9"]
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
      set pcolor green
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
      set pcolor green
      set source-energy Es
      set tag 0
    ]
    ]
    if regeneration-type = "deterministic" and tag = treg [
      set food-here? true
      set pcolor green
      set source-energy Es
      set tag 0
    ]
    if regeneration-type = "all-patches" [
      ifelse random-float 1 < xpgrow [
      set food-here? true
      set pcolor green
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
    ifelse estimate-food-neighbors = 1
    [move-to one-of neighbors with [food-here?]]
    [move-to one-of neighbors]
    set move? true
  ]
  if strategy = "S9" [
    if not [food-here?] of patch-here and estimate-food-neighbors = 1
    [move-to one-of neighbors with [food-here?] set move? true]
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
    ;if prev-s != strategy [
      ;set n-alpha 1
      ;set n-action 1
      ;set dE-estimate 0
      ;set dE-variance 0]
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

end
to update-strategy-accumulation
  ifelse rule = "imitation" or rule = "inquiring" [
    set-pool-of-strategies-accumulation
    set acc-limit one-of pool-of-strategies
    ifelse acc-limit = acc-limit-a [set color red][set color violet]
  ] [
  if rule = "optimizing" [
   set acc-limit acc-limit-b
   set color violet
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
  if member? strategy  ["S1" "S2" "S5" "S6"] [
       set pool-of-strategies remove 0 ([strategy] of turtles-here)]
  if member? strategy  ["S3" "S4" "S7" "S8" "S9"] [
      set pool-of-strategies remove 0 ([strategy] of turtles-on (patch-set patch-here neighbors))]
end

to set-pool-of-strategies-accumulation

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
@#$#@#$#@
GRAPHICS-WINDOW
308
45
803
541
-1
-1
11.9
1
10
1
1
1
0
0
0
1
-20
20
-20
20
1
1
1
ticks
30.0

BUTTON
21
16
76
49
setup
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

BUTTON
81
17
136
50
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
0

SLIDER
20
157
192
190
Mb
Mb
0.01
0.1
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
21
87
193
120
Es
Es
0
5
3.0
0.1
1
NIL
HORIZONTAL

SLIDER
20
122
195
155
Cm
Cm
0.001
0.09
0.02
0.001
1
NIL
HORIZONTAL

MONITOR
1147
468
1210
513
# agents
count persons
17
1
11

SLIDER
19
192
145
225
p-grow
p-grow
0.001
1
0.001
0.001
1
NIL
HORIZONTAL

SLIDER
21
52
193
85
CostP
CostP
0
1.0
0.01
0.01
1
NIL
HORIZONTAL

PLOT
811
37
1067
179
average satisfaction
NIL
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"sat-e" 1.0 0 -16777216 true "" "plot global-sat-e"
"sat-p" 1.0 0 -7500403 true "" "plot global-sat-p"
"sat-s" 1.0 0 -2674135 true "" "plot global-sat-s"
"sat-rate" 1.0 0 -955883 true "" "plot rate-satisfaction"

PLOT
1072
36
1362
179
average uncertainty
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"u" 1.0 0 -16777216 true "" "plot global-uncertainty"
"rate-u" 1.0 0 -7500403 true "" "plot rate-uncertainty"

PLOT
810
185
1064
360
population by strategy
NIL
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"S1" 1.0 0 -16777216 true "" "plot count turtles with [strategy = \"S1\"] / count persons"
"S2" 1.0 0 -7500403 true "" "plot count turtles with [strategy = \"S2\"] / count persons"
"S3" 1.0 0 -2674135 true "" "plot count turtles with [strategy = \"S3\"] / count persons"
"S4" 1.0 0 -955883 true "" "plot count turtles with [strategy = \"S4\"] / count persons"
"S5" 1.0 0 -6459832 true "" "plot count turtles with [strategy = \"S5\"] / count persons"
"S6" 1.0 0 -1184463 true "" "plot count turtles with [strategy = \"S6\"] / count persons"
"S7" 1.0 0 -10899396 true "" "plot count turtles with [strategy = \"S7\"] / count persons"
"S8" 1.0 0 -13840069 true "" "plot count turtles with [strategy = \"S8\"] / count persons"
"S9" 1.0 0 -14835848 true "" "plot count turtles with [strategy = \"S9\"] / count persons"

PLOT
1070
185
1360
359
population by rule
NIL
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"rep" 1.0 0 -2674135 true "" "plot pop-rep"
"imit" 1.0 0 -7500403 true "" "plot pop-imit"
"inq" 1.0 0 -955883 true "" "plot pop-inq"
"opt" 1.0 0 -6459832 true "" "plot pop-opt"

CHOOSER
20
227
146
272
regeneration-type
regeneration-type
"oasis" "vacancies" "deterministic" "all-patches"
3

SWITCH
1213
419
1370
452
history-record
history-record
1
1
-1000

CHOOSER
1213
370
1369
415
needs
needs
"complete-set" "only-existential"
1

CHOOSER
19
274
146
319
decision-rule-mode
decision-rule-mode
"dynamic" "one-rule"
0

CHOOSER
149
274
246
319
one-rule
one-rule
"inquiring" "repetition" "optimizing" "imitation"
0

CHOOSER
18
324
243
369
tournament
tournament
"all" "binary" "none" "accumulation" "accumulation-limit-binary" "self-rationing-accumulation"
0

CHOOSER
157
373
298
418
initial-strategy-a
initial-strategy-a
"S1" "S2" "S3" "S4" "S5" "S6" "S7" "S8" "S9"
8

SLIDER
16
377
145
410
acc-limit-a
acc-limit-a
0
100
54.0
1
1
NIL
HORIZONTAL

SLIDER
14
413
144
446
acc-limit-b
acc-limit-b
acc-limit-a
105
105.0
1
1
NIL
HORIZONTAL

CHOOSER
157
420
298
465
initial-strategy-b
initial-strategy-b
"S1" "S2" "S3" "S4" "S5" "S6" "S7" "S8" "S9"
3

SLIDER
1376
370
1487
403
pop-total
pop-total
10
1680
1680.0
1
1
NIL
HORIZONTAL

CHOOSER
12
485
146
530
sources-distribution
sources-distribution
"constant" "random-on-time" "random-on-space" "random-on-space-and-time"
0

CHOOSER
156
503
294
548
movement-type
movement-type
"simple" "composed"
1

CHOOSER
1213
491
1341
536
metabolism-type
metabolism-type
"lineal" "constant" "constant-lineal" "accumulation-punishment"
2

SLIDER
148
237
247
270
p-oasis
p-oasis
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
13
449
145
482
eating-fraction
eating-fraction
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
157
467
296
500
E0
E0
0
100
2.0
1
1
NIL
HORIZONTAL

SLIDER
1212
456
1340
489
Emax
Emax
E0
100
20.0
1
1
NIL
HORIZONTAL

BUTTON
147
19
222
52
go once
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

SLIDER
1375
443
1518
476
p-grow-abundance
p-grow-abundance
0.8
1.0
1.0
0.005
1
NIL
HORIZONTAL

SWITCH
1375
408
1518
441
famine-abundance
famine-abundance
0
1
-1000

SLIDER
1373
479
1517
512
p-grow-famine
p-grow-famine
0
0.2
0.0
0.005
1
NIL
HORIZONTAL

SLIDER
810
413
982
446
time-abundance
time-abundance
1
500
60.0
1
1
NIL
HORIZONTAL

SLIDER
809
454
980
487
time-famine
time-famine
0
500
40.0
1
1
NIL
HORIZONTAL

SWITCH
810
376
967
409
season-stochastic
season-stochastic
0
1
-1000

SLIDER
810
490
982
523
std-season
std-season
0
10
5.0
1
1
NIL
HORIZONTAL

MONITOR
992
372
1081
417
time abundance
time-abundance-t
2
1
11

MONITOR
1085
372
1165
417
time famine
time-famine-t
2
1
11

MONITOR
986
420
1070
465
season time
season-time
0
1
11

MONITOR
1147
420
1209
465
NIL
season-t
0
1
11

MONITOR
1074
420
1143
465
no. cycles
number-cycles
0
1
11

SWITCH
989
512
1147
545
final-abundance-stage
final-abundance-stage
0
1
-1000

SLIDER
990
473
1105
506
final-cycle
final-cycle
10
50
11.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This agent-based model aims to represent the cognitive dynamics on decision-making when agents take decisions on food consumption and how this depends on the richness of the environment which is a discrete grid of cells with energetic sources. An agent makes three binary decisions at a time step: to attempt to eat the sources on her current cell, to actively perceive her environment paying an energetic cost and to displace herself to a contiguous cell. These three decisions constitute an strategy and represent a plan but not the actual result that the agent gets. The agent deals with external and internal factors out of her control as the lack of food, the competition or the difficulties to move across the grid. To the eight possible combination of decision outputs we include an extra one which consists in attempting to eat, perceiving the environment seeking for food and moving if and only if sources are found. Agent's strategies are subject to change depending on agent's cognitive rules (or heuristics) which can be one of four: The repetition of the strategy, the imitation of any of the strategies observed in other agents, the selection of the best strategy observed in other agents by inquiring, or the selection of the best strategy by considering all of them (optimization). This set of cognitive rules are taken from the CONSUMAT model developed by Jager and Janssen (2002,2012). Decision rules can change according the satisfaction of needs in the agent and the uncertainty on the fulfillment of them. The original set of needs include existential, personality and social needs. In the code is possible to select to sets of needs: The original set of thee needs, or only the existential need. 
The environmental factors considered in the model are the energy that an agent can obtain exploiting a cell and the probability of regeneration of sources on empty cells. Is also possible to select if some portion of cells are permanently empty or full of energetic sources or if the regeneration occurs in a deterministic way, if is stochastic but constant, random on time, random on space or random on both.
The internal parameters of the agents define the way they execute the movement, the basal metabolism and the energetic cost of intentional actions. is also possible to create different "tournaments" to test agent's strategies and rules (this include different methods to store energy).
At time zero, the environment is set with one source of food on each cell and an initial population of agents distributed one on a different cell, with an specific distribution of rules and strategies and a diversity of tolerances to fail on need accomplishment. Each agent execute her strategy, evaluates her output, use her rule to select the following strategy and, if is the case, modify the current rule. If an agent reach zero energy, she is removed from the system. After agents perform all their actions and upgrades, each cell regenerates its energetic sources with a given probability and type of regeneration. 

## HOW TO USE IT

The system is tuned with the parameters listed below:

1. CostP: The energetic cost of active perception. It is a constant number.
2. Es: The energy provided by each energetic source.
3. Cm: Cost of moving. It is a constant number which is multiplied by the current energy of the agent.
4. Mb: Basal metabolism. It is a constant number which is multiplied by the maximum energy that every egent have ever reach.
5. p-grow: regeneration probability. It is a constant number which represents the probability to regenerate energetic sources in active sources if these are empty.
6. regeneration-type:This parameter specify how the cells regenerate. If is "deterministic" then every cell regenerates in an strict time t = 1 / p-grow. If is "oasis" then a portion equals to p-oasis is set to a probability of regeneration equal to p-grow while the remaining cells are set to a probability of regeneration of 0. If is "vacancies" then a portion equals to p-oasis is set to a probability of regeneration equal to p-grow while the remaining cells are set to a probability of regeneration of 1.
7. decision-rule-mode: If is equal to "dynamic", decision rules can change. Otherwise, ("one-rule") it is static.
8. one-rule: If decision-rule-mode is set as "one-rule", this selector set that unique static decision rule.
9. tournament: This selector specify many ways to compare the development of strategies at the beginning of the simulation, as if the strategies were competing between. If is equal to "all" every strategy is included on the initial configuration. If is "binary" only two strategies are introduced in the initial stage: initial-strategy-a and initial-strategy-b, each one with half of the initial population. If tournament is set to "none", only one random initial strategy is set at the starting point. 
If tournament is set to "accumulation" the tournament compares agents agents that accumulate energy and agents that only use energy for the immediate performed actions and waste the rest of the consumend energy. In this case, all the agents start with initial-strategy-a. If the tournament is set as "self-rationing-accumulation", the same as in "accumulation" happens, with the difference that the agent which does not accumulate extra energy, leave the remaing energetic sources on cell to other agents to use them. If tournament is equal to "accumulation-limit-binary" agents with different limit on accumulation of energy are compared. This limits are set with the variables acc-limit-a and acc-limit-b.  
10. sources-distribution: This selector set how sources are distributed on the environment by distributing the values of the probability of regeneration on cells. It can be constant, random on space, time or both. In all this cases the average regeneration time is fixed to the value pgrow.
11. movement-type: This selector determines if the lack or excess of energy alters the way an agent moves. If is "composed", an agent with an internal energy less than 2 or bigger than Emax would have considerably less chances to move.
12. metabolism-type. If is "lineal", every agent have a basal metabolim wich is lineal with the internal energy. If is "constant" the basal metabolism is the same for every agent. If is "constant-lineal" the metabolism is constant for agents with low internal energy but lineal for agents with internal energy above a thresehold of Emax. If is "accumulation-punishment", the metabolism is lineal with respect the maximum level of energy ever reach by the agent.
13. pop-total: Is the initial population.
14. famine-abundance: If famine-abundance is On, the environment experience a cycle of 
abundance where the probability of regeneration is set to "p-grow-abundance" (between 
0.8 and 1.0). Then the environment experience a cycle of famine where the probability of regeneration is set to "p_grow_famine"  (between 0.0 and 0.2). The duration of these 
periods can be fixed to the values "time-famine" and "time-abundance", of can be an 
stochastic variable, if "season-stochastic" is on, from a normal distribution centered
in the values "time-famine" and "time-abundance", and a standard deviation specified on
"std-season". If "final-abundance-stage" is on, the system ends in a permanent abundance
season after a number of cycles of abundance/famine equals to the variable "final-cycle". 

Click the SETUP button to setup agents and sources.

Click the GO button to start the simulation.

## THINGS TO NOTICE

The model allows different settings to make experiments on the environment features, the competition between strategies, the dynamics of rules, etc.

The set of strategies has 9 different strategies for completeness. But when the systems arrive to mature stages that look somehow stationary, in most scenarios, only few strategies survive. 

## THINGS TO TRY

Try to identify which conditions favour an specific strategy in binary tournaments. This can be done using strategies that differ in only one of the three decisions (for example S6 vs S8). Test if strategy S9 is, as assumed, a more rational strategy than S8.

Track the distribution of strategies on diferent environmental conditions, and see what happens when this conditions change abruptly.

To test the heuristic dynamic, the decision-rule-mode must be "dynamic" and tournament must be "all".

## RELATED MODELS

This model was created with the NetLogo Rabbits Grass Weeds model created by
Uri Wilensky as primary template.

* Wilensky, U. (2001).  NetLogo Rabbits Grass Weeds model.  http://ccl.northwestern.edu/netlogo/models/RabbitsGrassWeeds.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

The way in which needs and heuristics are defined are based on the CONSUMAT agent model described by Jager and Janssen (2002,2012).

* Jager, Wander & Janssen, Marco. (2002). The need for and development of behaviourally realistic agents. InInternationalWorkshop on Multi-Agent Systems and Agent-Based Simulation, (pp. 36–49). Springer.

* W. Jager and M. Janssen, “An updated conceptual framework for integrated modeling of humandecision making: The consumat ii,” inpaper for workshop complexity in the Real World@ ECCS,pp. 1–18, 2012.

Other models that use the CONSUMAT approach are listed below

* Janssen, Marco (2020, January 14). “Consumats on a network” (Version 1.0.0). CoMSES Computational Model Library. Retrieved from: https://www.comses.net/codebases/eed5f6aa-2b55-4f4c-9585-b4345345f82c/releases/1.0.0/

* Kalvas, František, Kudrnáčová, Michaela (2019, September 30). “Waste separation in small-world networks” (Version 1.0.0). CoMSES Computational Model Library. Retrieved from: https://www.comses.net/codebases/983e9017-f0da-47ae-ba99-d587b7444cbf/releases/1.0.0/

* Van Oel, Pieter (2015, April 13). “CONSERVAT” (Version 1.0.0). CoMSES Computational Model Library. Retrieved from: https://doi.org/10.25937/gtjr-3w27

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Batta, Erasmo & Stephens, Christopher (2020, July 1). "CONSUMAT agent system model of heuristic dynamics". CoMSES  Computational Model Library. 

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2001 Uri Wilensky.
![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227.

<!-- 2001 -->
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

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

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

rabbit
false
0
Circle -7500403 true true 76 150 148
Polygon -7500403 true true 176 164 222 113 238 56 230 0 193 38 176 91
Polygon -7500403 true true 124 164 78 113 62 56 70 0 107 38 124 91

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
<experiments>
  <experiment name="imitation_test1" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>pop</metric>
    <metric>pop1</metric>
    <metric>pop2</metric>
    <metric>pop3</metric>
    <metric>pop4</metric>
    <metric>pop5</metric>
    <metric>pop6</metric>
    <metric>pop7</metric>
    <metric>pop8</metric>
    <metric>pop9</metric>
    <metric>av-energy</metric>
    <metric>e1</metric>
    <metric>e2</metric>
    <metric>e3</metric>
    <metric>e4</metric>
    <metric>e5</metric>
    <metric>e6</metric>
    <metric>e7</metric>
    <metric>e8</metric>
    <metric>e9</metric>
    <metric>global-sat-e</metric>
    <metric>rate-satisfaction</metric>
    <metric>global-uncertainty</metric>
    <metric>rate-uncertainty</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;imitation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-grow" first="0.01" step="0.01" last="1"/>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="accumulation-famine-abundance" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>pop</metric>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>av-energy</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S2&quot;"/>
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="6"/>
      <value value="15"/>
      <value value="55"/>
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="40"/>
      <value value="90"/>
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="accumulation-famine-abundance_2" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>pop</metric>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>av-energy</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S2&quot;"/>
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="55"/>
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="accumulation-famine-abundance_3" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>pop</metric>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>av-energy</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S2&quot;"/>
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="6"/>
      <value value="15"/>
      <value value="55"/>
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="10"/>
      <value value="40"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="accumulation-famine-abundance_4_5" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>pop</metric>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>av-energy</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S2&quot;"/>
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="6"/>
      <value value="15"/>
      <value value="55"/>
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="famine_abundance_7_8" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>pop</metric>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>av-energy</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="famine_abundance_250_125" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>pop</metric>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>av-energy</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="famine_abundance_x" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>pop</metric>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>av-energy</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.3139"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="inquiring_test1" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>pop</metric>
    <metric>pop1</metric>
    <metric>pop2</metric>
    <metric>pop3</metric>
    <metric>pop4</metric>
    <metric>pop5</metric>
    <metric>pop6</metric>
    <metric>pop7</metric>
    <metric>pop8</metric>
    <metric>pop9</metric>
    <metric>av-energy</metric>
    <metric>e1</metric>
    <metric>e2</metric>
    <metric>e3</metric>
    <metric>e4</metric>
    <metric>e5</metric>
    <metric>e6</metric>
    <metric>e7</metric>
    <metric>e8</metric>
    <metric>e9</metric>
    <metric>global-sat-e</metric>
    <metric>rate-satisfaction</metric>
    <metric>global-uncertainty</metric>
    <metric>rate-uncertainty</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;inquiring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-grow" first="0.01" step="0.01" last="1"/>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="famine_abundance_250_125_2" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>pop</metric>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>av-energy</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S2&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="acc_vs_non_acc" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S2&quot;"/>
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
      <value value="&quot;imitation&quot;"/>
      <value value="&quot;inquiring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-grow" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="season_1" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;inquiring&quot;"/>
      <value value="&quot;imitation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-season">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="famine_abundance_250_125_2" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>pop</metric>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>av-energy</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S2&quot;"/>
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-season">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="famine_abundance_250_125_3" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>pop</metric>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>av-energy</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S2&quot;"/>
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;imitation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-season">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="famine_abundance_250_125_4" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>pop</metric>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>av-energy</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S2&quot;"/>
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
      <value value="&quot;imitation&quot;"/>
      <value value="&quot;inquiring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-season">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="famine_abundance_250_125_5" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>pop</metric>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>av-energy</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <metric>time-abundance-t</metric>
    <metric>time-famine-t</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S2&quot;"/>
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
      <value value="&quot;imitation&quot;"/>
      <value value="&quot;inquiring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-season">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;oasis&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="fam_ab_es_1" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <metric>time-abundance-t</metric>
    <metric>time-famine-t</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="165"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S2&quot;"/>
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="2"/>
      <value value="2.5"/>
      <value value="3"/>
      <value value="3.5"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
      <value value="&quot;imitation&quot;"/>
      <value value="&quot;inquiring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-season">
      <value value="0"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;all-patches&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="estrategias_ab60fam40" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S2&quot;"/>
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-season">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;all-patches&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="s_ab60fam40_imit" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S2&quot;"/>
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;imitation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-season">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;all-patches&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="s_ab60fam40_inq" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S2&quot;"/>
      <value value="&quot;S4&quot;"/>
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S8&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;inquiring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-season">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;all-patches&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="period_std_S6_S9_1" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <metric>time-abundance-t</metric>
    <metric>time-famine-t</metric>
    <metric>season-time</metric>
    <metric>season-t</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S6&quot;"/>
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
      <value value="&quot;imitation&quot;"/>
      <value value="&quot;inquiring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="std-season" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;all-patches&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="period_std_S9_2" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <metric>time-abundance-t</metric>
    <metric>time-famine-t</metric>
    <metric>season-time</metric>
    <metric>season-t</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
      <value value="&quot;imitation&quot;"/>
      <value value="&quot;inquiring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-season">
      <value value="0"/>
      <value value="1"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;all-patches&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="final_stage_abundance1" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1500"/>
    <metric>pop-acc-a</metric>
    <metric>pop-acc-b</metric>
    <metric>en-acc-a</metric>
    <metric>en-acc-b</metric>
    <metric>time-abundance-t</metric>
    <metric>time-famine-t</metric>
    <metric>season-time</metric>
    <metric>season-t</metric>
    <enumeratedValueSet variable="Cm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="season-stochastic">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-abundance">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="E0">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Emax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mb">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-a">
      <value value="&quot;S9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-oasis">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-type">
      <value value="&quot;composed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history-record">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="final-cycle">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Es">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tournament">
      <value value="&quot;accumulation-limit-binary&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="needs">
      <value value="&quot;only-existential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-rule">
      <value value="&quot;repetition&quot;"/>
      <value value="&quot;imitation&quot;"/>
      <value value="&quot;inquiring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-total">
      <value value="1680"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-famine">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sources-distribution">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-a">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-grow-abundance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famine-abundance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acc-limit-b">
      <value value="105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-strategy-b">
      <value value="&quot;S4&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eating-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-famine">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolism-type">
      <value value="&quot;constant-lineal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CostP">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-rule-mode">
      <value value="&quot;one-rule&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-type">
      <value value="&quot;all-patches&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-season">
      <value value="0"/>
      <value value="1"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="final-abundance-stage">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
@#$#@#$#@
0
@#$#@#$#@
