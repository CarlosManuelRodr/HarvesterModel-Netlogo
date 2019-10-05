extensions [r] ; used to setup the spatial distribution of the resource richness (carring capacity)

breed [cooperators cooperator]
breed [selfish a-selfish]

globals [ 
  num-movements ; number of movements of agents
  movecoop ; number of movements of cooperator agents
  moveselfi ; number of movements of selfish agents
  met ; metabolist of agents
  migration ; probability of agents moving to a random patch
  disthatch ; radious around patches as potential destinations for offsprings' settles
  growth-rate ; growth rate of resource
  k1 k2 k3 k4 k5
  fk1 fk2 fk3 fk4 fk5
  sumk1 sumk2 sumk3 sumk4 sumk5
  
  ;;Indicators
  avgset
  avgres
  avgpop
  avgcoop
  avgselfi
  avgcheat
  avgmov
  avgmovcoop
  avgmovselfi
  avgsetcoop
  avgsetselfi
  avgsumS 
  avgsumScoop 
  avgsumSselfi
  avgk1pop avgk2pop avgk3pop avgk4pop avgk5pop 
  avgk1set avgk2set avgk3set avgk4set avgk5set 
  avgk1res avgk2res avgk3res avgk4res avgk5res 
  avgk1cheat avgk2cheat avgk3cheat avgk4cheat avgk5cheat 
  avgk1coop avgk2coop avgk3coop avgk4coop avgk5coop 
  avgk1selfi avgk2selfi avgk3selfi avgk4selfi avgk5selfi 
   

  settlement ;patches occiped by at least one agent
  sumres
  sumpop
  sumcoop
  sumselfi
  sumS
  sumScoop
  sumSselfi
  summov
  summovecoop
  summoveselfi
  sumcheat
  k1pop k2pop k3pop k4pop k5pop
  k1set k2set k3set k4set k5set
  k1res k2res k3res k4res k5res
  k1cheat k2cheat k3cheat k4cheat k5cheat
  
  coop
  selfi
  k1coop k2coop k3coop k4coop k5coop 
  k1selfi k2selfi k3selfi k4selfi k5selfi 
  setcoop
  setselfi
  ]  

turtles-own [ 
  energy ; energy stored by agents
  S ; storage level. Variable related with how much each agent want to harvest
  oldstate
  dH ; desired harvest level of agents
  agH ; real harvest level of agents
  own-S ; own storage level of agents
  timeincell ; time steps agents are in the same cell
  old-S ; old storage level of agents
  agMSY ; harvest level envouraged by governments
  satisfied? ; if agents are satisfied
  cheater? ; TRUE if selfish agent harvest an amount larger than agMSY
  myk ; carring capacity of the patch 
  strategy ; whether the agent has a cooperate or selfish strategy
] 

patches-own [ 
  pophere ; number of agents in each patch
  resource ; resource level
  MSY ; sustainable harvest level in each patch
  k ; carrying capacity
  totharvest
  totrealharvest
] 

to setup   
  __clear-all-and-reset-ticks 
  r:clear
  resize-world 0 49 0 49
  set-patch-size 15

;; Initialize global variables
  set num-movements 0
  set movecoop 0
  set moveselfi 0
  set met 0.3
  set migration 0.1
  set disthatch 5
  set growth-rate 0.075

setup-landscape

;; Setup patches
   ask patches [
   set MSY k * growth-rate / 8   ; the harvest level of cooperator agents is an amount near to the maximum sustainable yield (MSY)
   set resource k / 2 ; resource is initialized at half of its carrying capacity
   set pophere count turtles-here
  ]

;; Setup agents
create-cooperators num-cooperators
[
  set size 1
  set shape "dot"
  set strategy "cooperate"
]

create-selfish num-selfish
[
  set size 0.7
  set shape "star"
  set strategy "selfish"
]

ask turtles [
  set color red
  set energy 10
  setxy int random-xcor int random-ycor ; agents are randomly distributed
  set timeincell 0
  set own-S random-float 1 ; storage level of agents are uniformly distributed
  set agMSY MSY / count turtles-here ; harvest level for each agent if cooperate strategy
  set satisfied? true
  set dH met * ( 1 + own-S) ; agents desire a havervest level higher than the minimum required to meet their metabolism 
  set myk [k] of patch-here
  ]

;; Define the views of the world
  if view = "resource" [ask patches [ set pcolor scale-color green resource 140 10 ]]
  if view = "population" [ask patches [set pcolor scale-color pink pophere 15 0 ]]
  if view = "k" [ask patches [set pcolor scale-color yellow k 140 10]] 

;; Initialize indicators
  set settlement count (patches with [any? turtles-here]) / 2500
  ifelse k? = "default" [let totres 250000 set sumres sum [resource] of patches / totres] [
    let totres ((k1 * count patches with [k = k1]) + (k2 * count patches with [k = k2]) + (k3 * count patches with [k = k3]) + (k4 * count patches with [k = k4]) + (k5 * count patches with [k = k5]) ) 
    set sumres sum [resource] of patches / totres ]
 
  set avgres 0
  set sumpop count turtles
  set sumcoop count cooperators
  set sumselfi count selfish
  set avgpop 0
  set avgset 0
  set sumS (mean [own-S] of turtles) 
  set sumScoop (mean [own-S] of cooperators)
  set sumSselfi (mean [own-S] of selfish)
  set summov 0
  set summovecoop 0
  set summoveselfi 0
  set sumcheat 0
  
  set coop count cooperators / count turtles
  set selfi count selfish / count turtles
  set setcoop count (patches with [any? cooperators-here]) / 2500
  set setselfi count (patches with [any? selfish-here]) / 2500
  
  if k? = "uniform" [
  set k1pop count turtles with [myk = k1] / count turtles 
  set k2pop count turtles with [myk = k2] / count turtles 
  set k3pop count turtles with [myk = k3] / count turtles 
  set k4pop count turtles with [myk = k4] / count turtles 
  set k5pop count turtles with [myk = k5] / count turtles
  set k1set count patches with [pophere > 0 and k = k1] / sumk1 
  set k2set count patches with [pophere > 0 and k = k2] / sumk2
  set k3set count patches with [pophere > 0 and k = k3] / sumk3
  set k4set count patches with [pophere > 0 and k = k4] / sumk4
  set k5set count patches with [pophere > 0 and k = k5] / sumk5
      
  set k1res sum [resource] of patches with [k = k1] / (k1 * count patches with [k = k1])
  set k2res sum [resource] of patches with [k = k2] / (k2 * count patches with [k = k2])
  set k3res sum [resource] of patches with [k = k3] / (k3 * count patches with [k = k3])
  set k4res sum [resource] of patches with [k = k4] / (k4 * count patches with [k = k4])
  set k5res sum [resource] of patches with [k = k5] / (k5 * count patches with [k = k5])
  set k1cheat 0
  set k2cheat 0 
  set k3cheat 0
  set k4cheat 0 
  set k5cheat 0

  set k1coop count cooperators with [myk = k1] / count cooperators
  set k2coop count cooperators with [myk = k2] / count cooperators
  set k3coop count cooperators with [myk = k3] / count cooperators
  set k4coop count cooperators with [myk = k4] / count cooperators
  set k5coop count cooperators with [myk = k5] / count cooperators
  set k1selfi count selfish with [myk = k1] / count selfish
  set k2selfi count selfish with [myk = k2] / count selfish
  set k3selfi count selfish with [myk = k3] / count selfish
  set k4selfi count selfish with [myk = k4] / count selfish
  set k5selfi count selfish with [myk = k5] / count selfish
  ]

  update-outputs
end 

to go
  tick
  if not any? turtles [ stop ]
  if sum [resource] of patches <= 0.001 [ stop ]
  if ticks > years-simulated [ stop ]
 
  ask patches [set pophere count turtles-here]  
  move
  ask patches [set pophere count turtles-here] 
  ask turtles [set myk [k] of patch-here]
     
  select-harvest
  harvest
  pay-fee
  define-insatisfied
  select-storage
  select-strategy
  mutate
  grow-resource
 
;; Update the views of the world    
  if view = "resource" [ask patches [ set pcolor scale-color green resource 140 10 ]]
  if view = "population" [ask patches [set pcolor scale-color pink pophere 15 0 ]]
  if view = "k" [ask patches [set pcolor scale-color yellow k 140 10]]
  
  popdynamic
  update-outputs
end 

to move 
  set num-movements 0
  set movecoop 0
  set moveselfi 0
  ask turtles [
    set dH met + met * own-S 
    set timeincell timeincell + 1
    if ( resource / pophere ) < dH ; if there are not enough resource to satisfied the desired harvesting level of agents
      [  
        if random-float 1 < pMove [  ; with a certain probability dissatisfied agents move to another patch
          let destination patches in-radius move-capacity with-max [ resource ] ; agents move to the closest patch with max resource within a move-capacity radius
          set destination min-one-of destination [distance myself] 
          if [resource] of destination > resource [                   
            move-to destination
            set timeincell 0
            set num-movements num-movements + 1
            if strategy = "cooperate" [set movecoop movecoop + 1]
            if strategy = "selfish" [set moveselfi moveselfi + 1]
            set energy energy - met * 2 ; move cost energy     
          ]
        ]                    
      ]
    if random-float 1 < migration ; besides movement due to dissatisfaction, agents can move to another random patch
      [ move-to one-of patches 
        set timeincell 0
        ;set num-movements num-movements + 1 ; we don't sum the movements that are not based on dissatisfaction
        set energy energy - met  * 2 ; move cost energy
      ]
    if energy <= 0 [die]
    ;;change color of moving agents
    ifelse timeincell >= 1 [ set color red ][ set color blue ]
  ]
  ;show num-movements
end  

;; agents decide their harvest level
to select-harvest
  
  ;; each patch has a certain amount close to the maximum sustainable yield as maximum harvesting level for cooperators
  ask patches [
    set MSY k * growth-rate / 8 
    if MSY > resource [set MSY resource]
    if resource <= 0 [set MSY 0]
  ]
  
  ask cooperators [
    set agH 0
    set agMSY 0
    set agMSY MSY / pophere ; harvest level for cooperators
    ifelse dH <= agMSY 
    [ ; if the desired harvest level of agents is egual or less than the sustainable harvest level 
      set agH dH set cheater? false
    ]
    [
      set agH agMSY set cheater? false
    ]
  ]
  
    ask selfish [
    set agH 0
    set agMSY 0
    set agMSY MSY / pophere ; harvest level for cooperators
    ifelse dH <= agMSY 
    [ ; if the desired harvest level of agents is egual or less than the sustainable harvest level 
      set agH dH set cheater? false
    ]
    [
      set agH dH set cheater? true 
    ]
  ]
end

;; agents harvest
to harvest
  ask patches [ ifelse pophere > 0 [ set totharvest sum [agH] of turtles-here] [set totharvest 0]
  ]  
  ask turtles [
    if totharvest > resource 
    [set agH agH - ((agH * (totharvest - resource)) / totharvest) ] ;; if there is no enought resource, a proportional reduction of harvesting level is applied
    if agH < 0 [set agH 0] 
    set energy energy + agH - met 
  ]
end
  
to pay-fee
  ;show count turtles with [agH > agMSY] / count turtles
  ask turtles [
    if agH > agMSY 
    [ if random-float 1 < pCatch ; if cheaters are catch they pay a fee
      [ 
        set energy energy - 2 * (agH - agMSY)
      ]
    ]
      
  ]
end

to define-insatisfied
  
  ask turtles [ 
    ifelse agH < dH ;; agents are not satisfied
    [ ifelse random-float 1 < own-S 
      [ set satisfied? false 
        ;print "insatisfied!!"
        ]
      [ set satisfied? true ]
      ;print "satisfied"
      
    ]
    [
      set satisfied? true 
      ;print "satisfied!!"
    ]
  ]
end

;; not satisfied agents copy the storage-rate of other agents with the highest fitness (energy level)
to select-storage

  ask turtles [ 
    if pophere > 1 [
      set oldstate energy     
      set old-S own-S  
      if (random-float 1 < ia) [ ; ia= imitation agents
        if satisfied? = false [
          
          let max-fitness max-one-of other turtles-here [ oldstate ]
          if oldstate < [oldstate] of max-fitness [  ; copy other agents that are doing better          
            
            if [old-S] of max-fitness != old-S [
              
              if [dH ] of max-fitness - [agH] of max-fitness < dH - agH [ ; copy the storage of the agents that are more satisfied  
                set own-S [ old-S] of max-fitness
                ;print "coping!"
              ]
            ]
          ]
        ]
      ]
    ]
  ]
end

;; not satisfied agents copy the strategy of other agents with the highest fitness (energy level)
to select-strategy
   ask turtles [ 
    if pophere > 1 [
      let old-strategy strategy
      if (random-float 1 < ia) [ ; ia= imitation agents
        if satisfied? = false [
          let max-fitness max-one-of other turtles-here [ oldstate ]
          if oldstate < [oldstate] of max-fitness [  ; copy other agents that are doing better          
            if strategy != [strategy] of max-fitness [
            set strategy [strategy] of max-fitness
            ifelse old-strategy = "cooperate" and strategy = "selfish" [set breed selfish set shape "star" set size 0.7 
              ;show "coop-selfi" 
              ] [set breed cooperators set shape "dot" set size 1 
              ;show "selfi-coop"
              ]
            ]
            
          ]
        ]
      ]
    ]
   ]
end

to mutate
  
  ask turtles [
    let old-strategy strategy
    if random-float 1 < pMutate [ ;mutation
      ifelse old-strategy = "cooperate" [
        set breed selfish set shape "star" set size 0.7 set strategy "selfish"
      ] [
      set breed cooperators set shape "dot" set size 1 set strategy "cooperate"
      ]
    ]
  ]
end


;; density-dependent growth of resource
to grow-resource 

  ask patches [ 
    set totrealharvest 0 if pophere > 0 [ set totrealharvest totrealharvest + sum [agH] of turtles-here]
  ]
  
  ask patches [
    ifelse k > 0 [
    set resource resource - totrealharvest + ( growth-rate * resource * (1 - resource / k) )] [set resource 0]
    if resource < 0.0 [set resource 0.001]
  ]
end

;; agents reproduce and die
to popdynamic
  ask turtles [
    if energy <= 0 [ die ] 
      let birthrate 0.02
       if random-float 1.0 < birthrate * (energy / 100.0) [ ; birth rate of agents depends on their stock of energy 
        set energy (energy / 2)
        hatch 1 [
         let potential-destination patches in-radius disthatch with-max [resource] 
         set potential-destination min-one-of potential-destination [ distance myself ] 
         move-to potential-destination
         set satisfied? true
         set timeincell 0
         set color yellow
         ifelse strategy = "cooperate" [set size 1 set shape "dot"] [set size 0.7 set shape "star"]
          ]
      ]
  ]
end

to update-outputs
  if plots? [
    if count turtles > 0 [

      set-current-plot "k"
      set-current-plot-pen "hist-k"
      set-histogram-num-bars 5
      histogram ([k] of patches)
      
      set-current-plot "Population"
      set-current-plot-pen "agents"
      plot (count turtles)
      
      set-current-plot "Settlements"
      set-current-plot-pen "set"
      plot settlement * 100
      
      let resourcetot 0
      set-current-plot "Resource"
      set-current-plot-pen "resource"
      set-plot-pen-color green
      plot ( (sum [resource] of patches / 250000 ) * 100)
      
      set-current-plot "Movement" 
      set-current-plot-pen "movement"
      plot (num-movements) 
      set-current-plot-pen "coopmovement"
      plot (movecoop)      
      set-current-plot-pen "selfimovement"
      plot (moveselfi) 
        
      set-current-plot "Harvesting"
      set-current-plot-pen "harvesting"
      plot (mean [agH] of turtles )  
      set-plot-pen-color black
      set-current-plot-pen "sustainable"
      plot (mean [agMSY] of turtles)
      set-plot-pen-color green
      set-current-plot-pen "expected"
      plot (mean [dH] of turtles)
      
      set-current-plot "% Cheaters" 
      set-current-plot-pen "selfish"
      plot ((count selfish with [cheater? = true] / count selfish) * 100)  ;;
      set-current-plot-pen "agents"
      plot ((count selfish with [cheater? = true] / count turtles) * 100)  ;;
      
      set-current-plot "Storage" 
      set-current-plot-pen "turtles"
      plot ( sumS) 
      set-current-plot-pen "coop"
      ifelse count cooperators > 0 [plot ( sumScoop) ] [plot 0]
      set-current-plot-pen "selfish"
      plot ( sumSselfi) 
;      set-current-plot-pen "min"
;      plot ( min [own-S] of turtles) 
;      set-current-plot-pen "max"
;      plot ( max [own-S] of turtles) 
 
 
 set-current-plot "Type of agent"
 set-current-plot-pen "cooperator"
 ifelse count turtles > 0 [plot count cooperators / count turtles] [plot 0]
 set-current-plot-pen "selfish"
 ifelse count turtles > 0 [plot count selfish / count turtles] [plot 0]
    ]
  ]

  
;; indicators

;;All scenarios
  ifelse count turtles > 0 [set settlement count (patches with [any? turtles-here]) / 2500] [set settlement 0]
  ifelse k? = "default" [set sumres sum [resource] of patches / 250000] 
  [set sumres sum [resource] of patches / ((k1 * count patches with [k = k1]) + (k2 * count patches with [k = k2]) + (k3 * count patches with [k = k3]) + (k4 * count patches with [k = k4]) + (k5 * count patches with [k = k5]) ) ]
    
  set sumpop count turtles
  set sumcoop count cooperators
  set sumselfi count selfish
  ifelse count turtles > 0 [set summov num-movements / count turtles ] [set summov 0]
  ifelse count cooperators > 0 [set summovecoop movecoop / count cooperators] [set summovecoop 0]
  ifelse count selfish > 0 [set summoveselfi moveselfi / count selfish] [set summoveselfi 0]
 
  ifelse count turtles > 0 [set sumS (mean [own-S] of turtles)] [set sumS "NA"]
  ifelse count cooperators > 0 [set sumScoop (mean [own-S] of cooperators)] [set sumScoop "NA"]
  ifelse count selfish > 0 [set sumSselfi (mean [own-S] of selfish)] [set sumSselfi "NA"]
  ifelse count turtles > 0 [set sumcheat count turtles with [cheater? = true] / count turtles] [set sumcheat 0]
  ifelse count cooperators > 0 [set coop count cooperators / count turtles] [set coop 0]
  ifelse count selfish > 0 [set selfi count selfish / count turtles] [set selfi 0]
 
  ifelse count cooperators > 0 [set setcoop count (patches with [any? cooperators-here]) / 2500] [set setcoop 0]
  ifelse count selfish > 0 [set setselfi count (patches with [any? selfish-here]) / 2500] [set setselfi 0]
 
  if k? = "uniform" [
    ifelse count turtles > 0 [set k1pop count turtles with [myk = k1] / count turtles] [set k1pop 0]
    ifelse count turtles > 0 [set k2pop count turtles with [myk = k2] / count turtles] [set k2pop 0]
    ifelse count turtles > 0 [set k3pop count turtles with [myk = k3] / count turtles] [set k3pop 0]
    ifelse count turtles > 0 [set k4pop count turtles with [myk = k4] / count turtles] [set k4pop 0]
    ifelse count turtles > 0 [set k5pop count turtles with [myk = k5] / count turtles] [set k5pop 0]
      
    set k1set count patches with [pophere > 0 and k = k1] / sumk1 
    set k2set count patches with [pophere > 0 and k = k2] / sumk2
    set k3set count patches with [pophere > 0 and k = k3] / sumk3
    set k4set count patches with [pophere > 0 and k = k4] / sumk4
    set k5set count patches with [pophere > 0 and k = k5] / sumk5
      
    set k1res sum [resource] of patches with [k = k1] / (k1 * count patches with [k = k1])
    set k2res sum [resource] of patches with [k = k2] / (k2 * count patches with [k = k2])
    set k3res sum [resource] of patches with [k = k3] / (k3 * count patches with [k = k3])
    set k4res sum [resource] of patches with [k = k4] / (k4 * count patches with [k = k4])
    set k5res sum [resource] of patches with [k = k5] / (k5 * count patches with [k = k5])
  
    ifelse count cooperators with [myk = k1] > 0 [set k1coop count cooperators with [myk = k1] / count cooperators] [set k1coop 0]
    ifelse count cooperators with [myk = k2] > 0 [set k2coop count cooperators with [myk = k2] / count cooperators] [set k2coop 0]
    ifelse count cooperators with [myk = k3] > 0 [set k3coop count cooperators with [myk = k3] / count cooperators] [set k3coop 0]
    ifelse count cooperators with [myk = k4] > 0 [set k4coop count cooperators with [myk = k4] / count cooperators] [set k4coop 0]
    ifelse count cooperators with [myk = k5] > 0 [set k5coop count cooperators with [myk = k5] / count cooperators] [set k5coop 0]
    ifelse count selfish with [myk = k1] > 0 [set k1selfi count selfish with [myk = k1] / count selfish] [set k1selfi 0]
    ifelse count selfish with [myk = k2] > 0 [set k2selfi count selfish with [myk = k2] / count selfish] [set k2selfi 0]
    ifelse count selfish with [myk = k3] > 0 [set k3selfi count selfish with [myk = k3] / count selfish] [set k3selfi 0]
    ifelse count selfish with [myk = k4] > 0 [set k4selfi count selfish with [myk = k4] / count selfish] [set k4selfi 0]
    ifelse count selfish with [myk = k5] > 0 [set k5selfi count selfish with [myk = k5] / count selfish] [set k5selfi 0]
    
    ifelse count selfish with [cheater? = TRUE and myk = k1] > 0 [set k1cheat count selfish with [cheater? = TRUE and myk = k1] / count selfish] [set k1cheat 0]
    ifelse count selfish with [cheater? = TRUE and myk = k2] > 0 [set k2cheat count selfish with [cheater? = TRUE and myk = k2] / count selfish] [set k2cheat 0]
    ifelse count selfish with [cheater? = TRUE and myk = k3] > 0 [set k3cheat count selfish with [cheater? = TRUE and myk = k3] / count selfish] [set k3cheat 0]
    ifelse count selfish with [cheater? = TRUE and myk = k4] > 0 [set k4cheat count selfish with [cheater? = TRUE and myk = k4] / count selfish] [set k4cheat 0]
    ifelse count selfish with [cheater? = TRUE and myk = k5] > 0 [set k5cheat count selfish with [cheater? = TRUE and myk = k5] / count selfish] [set k5cheat 0]
    
    
  ]


;;AVERAGE
  if ticks > (years-simulated - 1000) [ 
    set avgset avgset + (count (patches with [any? turtles-here]) / 2500) * 0.001
    set avgpop avgpop + count turtles * 0.001 
    ifelse count cooperators > 0 [set avgcoop avgcoop + (count cooperators / count turtles) * 0.001 ] [set avgcoop avgcoop]
    set avgselfi avgselfi + (count selfish / count turtles) * 0.001 
    
    set avgcheat avgcheat + (count selfish with [cheater? = true] / count selfish) * 0.001 ;;
    
    ifelse k? = "default" [set avgres avgres + (sum [resource] of patches / 250000) * 0.001] 
    [let totres ((k1 * count patches with [k = k1]) + (k2 * count patches with [k = k2]) + (k3 * count patches with [k = k3]) + (k4 * count patches with [k = k4]) + (k5 * count patches with [k = k5]) ) 
      set avgres avgres + (sum [resource] of patches / totres) * 0.001]
    
    set avgmov avgmov + summov * 0.001
    ifelse count cooperators > 0 [set avgmovcoop avgmovcoop + summovecoop * 0.001] [set avgmovcoop avgmovcoop]
    set avgmovselfi avgmovselfi + summoveselfi * 0.001
    
    set avgsetcoop avgsetcoop + (count (patches with [any? cooperators-here]) / 2500) * 0.001
    set avgsetselfi avgsetselfi + (count (patches with [any? selfish-here]) / 2500) * 0.001
    set avgsumS avgsumS + (mean [own-S] of turtles) * 0.001
    ifelse count cooperators > 0 [set avgsumScoop avgsumScoop + (mean [own-S] of cooperators) * 0.001] [set avgsumScoop avgsumScoop]
    set avgsumSselfi avgsumSselfi + (mean [own-S] of selfish) * 0.001

  if k? = "uniform" [
    set avgk1pop avgk1pop + (count turtles with [myk = k1] / count turtles) * 0.001
    set avgk2pop avgk2pop + (count turtles with [myk = k2] / count turtles) * 0.001
    set avgk3pop avgk3pop + (count turtles with [myk = k3] / count turtles) * 0.001
    set avgk4pop avgk4pop + (count turtles with [myk = k4] / count turtles) * 0.001
    set avgk5pop avgk5pop + (count turtles with [myk = k5] / count turtles) * 0.001
     
    set avgk1set avgk1set + (count patches with [pophere > 0 and k = k1] / sumk1) * 0.001 
    set avgk2set avgk2set + (count patches with [pophere > 0 and k = k2] / sumk2) * 0.001
    set avgk3set avgk3set + (count patches with [pophere > 0 and k = k3] / sumk3) * 0.001
    set avgk4set avgk4set + (count patches with [pophere > 0 and k = k4] / sumk4) * 0.001
    set avgk5set avgk5set + (count patches with [pophere > 0 and k = k5] / sumk5) * 0.001
      
    set avgk1res avgk1res + (sum [resource] of patches with [k = k1] / (k1 * count patches with [k = k1])) * 0.001
    set avgk2res avgk2res + (sum [resource] of patches with [k = k2] / (k2 * count patches with [k = k2])) * 0.001
    set avgk3res avgk3res + (sum [resource] of patches with [k = k3] / (k3 * count patches with [k = k3])) * 0.001
    set avgk4res avgk4res + (sum [resource] of patches with [k = k4] / (k4 * count patches with [k = k4])) * 0.001
    set avgk5res avgk5res + (sum [resource] of patches with [k = k5] / (k5 * count patches with [k = k5])) * 0.001
  
    ifelse count turtles with [myk = k1] > 0 [set avgk1cheat avgk1cheat + (count turtles with [cheater? = true and myk = k1] / count turtles with [myk = k1]) * 0.001][set avgk1cheat avgk1cheat]
    ifelse count turtles with [myk = k2] > 0 [set avgk2cheat avgk2cheat + (count turtles with [cheater? = true and myk = k2] / count turtles with [myk = k2]) * 0.001][set avgk2cheat avgk2cheat]
    ifelse count turtles with [myk = k3] > 0 [set avgk3cheat avgk3cheat + (count turtles with [cheater? = true and myk = k3] / count turtles with [myk = k3]) * 0.001][set avgk3cheat avgk3cheat]
    ifelse count turtles with [myk = k4] > 0 [set avgk4cheat avgk4cheat + (count turtles with [cheater? = true and myk = k4] / count turtles with [myk = k4]) * 0.001][set avgk4cheat avgk4cheat]
    ifelse count turtles with [myk = k5] > 0 [set avgk5cheat avgk5cheat + (count turtles with [cheater? = true and myk = k5] / count turtles with [myk = k5]) * 0.001][set avgk5cheat avgk5cheat]
    
    ifelse count cooperators with [myk = k1] > 0 [set avgk1coop avgk1coop + (count cooperators with [myk = k1] / count cooperators) * 0.001] [set k1coop k1coop]
    ifelse count cooperators with [myk = k2] > 0 [set avgk2coop avgk2coop + (count cooperators with [myk = k2] / count cooperators) * 0.001] [set k2coop k2coop]
    ifelse count cooperators with [myk = k3] > 0 [set avgk3coop avgk3coop + (count cooperators with [myk = k3] / count cooperators) * 0.001] [set k3coop k3coop]
    ifelse count cooperators with [myk = k4] > 0 [set avgk4coop avgk4coop + (count cooperators with [myk = k4] / count cooperators) * 0.001] [set k4coop k4coop]
    ifelse count cooperators with [myk = k5] > 0 [set avgk5coop avgk5coop + (count cooperators with [myk = k5] / count cooperators) * 0.001] [set k5coop k5coop]
    ifelse count selfish with [myk = k1] > 0 [set avgk1selfi avgk1selfi + (count selfish with [myk = k1] / count selfish) * 0.001] [set k1selfi k1selfi]
    ifelse count selfish with [myk = k2] > 0 [set avgk2selfi avgk2selfi + (count selfish with [myk = k2] / count selfish) * 0.001] [set k2selfi k2selfi]
    ifelse count selfish with [myk = k3] > 0 [set avgk3selfi avgk3selfi + (count selfish with [myk = k3] / count selfish) * 0.001] [set k3selfi k3selfi]
    ifelse count selfish with [myk = k4] > 0 [set avgk4selfi avgk4selfi + (count selfish with [myk = k4] / count selfish) * 0.001] [set k4selfi k4selfi]
    ifelse count selfish with [myk = k5] > 0 [set avgk5selfi avgk5selfi + (count selfish with [myk = k5] / count selfish) * 0.001] [set k5selfi k5selfi]
]
]




end

;; Using the r-extension, define four different landscape structures based on the distribution of resource richness (homogeneous, uniform, normal, and exponential)
to setup-landscape

  ask patches [ 
    set k 0 
    if k? = "default" [set k 100]] ; in the default landscape, carriging capacity of resource is homegeneously distributed
  if k? = "uniform" [
    set k1 60
    set k2 80
    set k3 100
    set k4 120
    set k5 140
    set sumk1 500
    set sumk2 500
    set sumk3 500
    set sumk4 500
    set sumk5 500
    
    ask n-of 500 patches [set k k1] 
    ask n-of 500 patches with [k = 0] [set k k2] 
    ask n-of 500 patches with [k = 0] [set k k3] 
    ask n-of 500 patches with [k = 0] [set k k4] 
    ask n-of 500 patches with [k = 0] [set k k5] 
  ]
  if k? = "normal" [ ; in the "normal" landscape, carriging capacity of resource is normally distributed
    r:eval "nor<-rnorm(2500)"
    r:eval "nor<-sort(nor-(min(nor)))"
    r:eval "nordis<-cut(nor,5)"
    r:eval "freqnordis<-as.data.frame(table(unclass(nordis)))"
    r:eval "freqnordis<-freqnordis$Freq"
    r:eval "fnor1<-freqnordis[1]"
    r:eval "fnor2<-freqnordis[2]"
    r:eval "fnor3<-freqnordis[3]"
    r:eval "fnor4<-freqnordis[4]"
    r:eval "fnor5<-freqnordis[5]"
    r:eval "fnor<-c(fnor1,fnor2,fnor3,fnor4,fnor5)"
    r:eval "nor1<-mean(nor[0:freqnordis[1]])"
    r:eval "nor2<-mean(nor[freqnordis[1]+1:freqnordis[2]])"
    r:eval "nor3<-mean(nor[freqnordis[2]+freqnordis[1]+1:freqnordis[3]])"
    r:eval "nor4<-mean(nor[freqnordis[3]+freqnordis[2]+freqnordis[1]+1:freqnordis[4]])"
    r:eval "nor5<-mean(nor[freqnordis[4]+freqnordis[3]+freqnordis[2]+freqnordis[1]+1:freqnordis[5]])"
    r:eval "sum<-sum(nor1*fnor1,nor2*fnor2,nor3*fnor3,nor4*fnor4,nor5*fnor5)"
    r:eval "inor1<-nor1*250000/sum"
    r:eval "inor2<-nor2*250000/sum"
    r:eval "inor3<-nor3*250000/sum"
    r:eval "inor4<-nor4*250000/sum"
    r:eval "inor5<-nor5*250000/sum"
    r:eval "valnordis<-c(inor1,inor2,inor3,inor4,inor5)"

    let fnor1 r:get "fnor1" 
    let fnor2 r:get "fnor2" 
    let fnor3 r:get "fnor3" 
    let fnor4 r:get "fnor4" 
    let fnor5 r:get "fnor5" 
       
    set fk1 r:get "fnor1"
    set fk2 r:get "fnor2"
    set fk3 r:get "fnor3"
    set fk4 r:get "fnor4"
    set fk5 r:get "fnor5"
    
    set k1 r:get "inor1"
    set k2 r:get "inor2"
    set k3 r:get "inor3"
    set k4 r:get "inor4"
    set k5 r:get "inor5"
   
    ask n-of fnor1 patches with [k = 0] [set k k1]
    ask n-of fnor2 patches with [k = 0] [set k k2]
    ask n-of fnor3 patches with [k = 0] [set k k3]
    ask n-of fnor4 patches with [k = 0] [set k k4]
    ask n-of fnor5 patches with [k = 0] [set k k5]

    set sumk1 count patches with [k = k1]
    set sumk2 count patches with [k = k2]
    set sumk3 count patches with [k = k3]
    set sumk4 count patches with [k = k4]
    set sumk5 count patches with [k = k5]
    ]

  if k? = "exponential" [ ; in the exponential landscape, carriging capacity of resource is exponentially distributed
    r:eval "exp<-rexp(2500)"
    r:eval "exp<-sort(exp-(min(exp)))"
    r:eval "expdis<-cut(exp,5)"
    r:eval "freqexpdis<-as.data.frame(table(unclass(expdis)))"
    r:eval "freqexpdis<-freqexpdis$Freq"
    r:eval "fexp1<-freqexpdis[1]"
    r:eval "fexp2<-freqexpdis[2]"
    r:eval "fexp3<-freqexpdis[3]"
    r:eval "fexp4<-freqexpdis[4]"
    r:eval "fexp5<-freqexpdis[5]"
    r:eval "fexp<-c(fexp1,fexp2,fexp3,fexp4,fexp5)"
    r:eval "exp1<-mean(exp[0:freqexpdis[1]])"
    r:eval "exp2<-mean(exp[freqexpdis[1]+1:freqexpdis[2]])"
    r:eval "exp3<-mean(exp[freqexpdis[2]+freqexpdis[1]+1:freqexpdis[3]])"
    r:eval "exp4<-mean(exp[freqexpdis[3]+freqexpdis[2]+freqexpdis[1]+1:freqexpdis[4]])"
    r:eval "exp5<-mean(exp[freqexpdis[4]+freqexpdis[3]+freqexpdis[2]+freqexpdis[1]+1:freqexpdis[5]])"
    r:eval "sum<-sum(exp1*fexp1,exp2*fexp2,exp3*fexp3,exp4*fexp4,exp5*fexp5)"
    r:eval "iexp1<-exp1*250000/sum"
    r:eval "iexp2<-exp2*250000/sum"
    r:eval "iexp3<-exp3*250000/sum"
    r:eval "iexp4<-exp4*250000/sum"
    r:eval "iexp5<-exp5*250000/sum"
    r:eval "valexpdis<-c(iexp1,iexp2,iexp3,iexp4,iexp5)"

    let fexp1 r:get "fexp1" 
    let fexp2 r:get "fexp2" 
    let fexp3 r:get "fexp3" 
    let fexp4 r:get "fexp4" 
    let fexp5 r:get "fexp5" 
   
    ifelse fexp5 > 0 [ ; we reapet the code to make sure that the highest level of k is included
      set fk1 r:get "fexp1"
      set fk2 r:get "fexp2"
      set fk3 r:get "fexp3"
      set fk4 r:get "fexp4"
      set fk5 r:get "fexp5"
       
    set k1 r:get "iexp1"
    set k2 r:get "iexp2"
    set k3 r:get "iexp3"
    set k4 r:get "iexp4"
    set k5 r:get "iexp5"
   
    ask n-of fexp1 patches with [k = 0] [set k k1]
    ask n-of fexp2 patches with [k = 0] [set k k2]
    ask n-of fexp3 patches with [k = 0] [set k k3]
    ask n-of fexp4 patches with [k = 0] [set k k4]
    ask n-of fexp5 patches with [k = 0] [set k k5]

    set sumk1 count patches with [k = k1]
    set sumk2 count patches with [k = k2]
    set sumk3 count patches with [k = k3]
    set sumk4 count patches with [k = k4]
    set sumk5 count patches with [k = k5]
    ] 
    
    [
    r:eval "exp<-rexp(2500)"
    r:eval "exp<-sort(exp-(min(exp)))"
    r:eval "expdis<-cut(exp,5)"
    r:eval "freqexpdis<-as.data.frame(table(unclass(expdis)))"
    r:eval "freqexpdis<-freqexpdis$Freq"
    r:eval "fexp1<-freqexpdis[1]"
    r:eval "fexp2<-freqexpdis[2]"
    r:eval "fexp3<-freqexpdis[3]"
    r:eval "fexp4<-freqexpdis[4]"
    r:eval "fexp5<-freqexpdis[5]"
    r:eval "fexp<-c(fexp1,fexp2,fexp3,fexp4,fexp5)"
    r:eval "exp1<-mean(exp[0:freqexpdis[1]])"
    r:eval "exp2<-mean(exp[freqexpdis[1]+1:freqexpdis[2]])"
    r:eval "exp3<-mean(exp[freqexpdis[2]+freqexpdis[1]+1:freqexpdis[3]])"
    r:eval "exp4<-mean(exp[freqexpdis[3]+freqexpdis[2]+freqexpdis[1]+1:freqexpdis[4]])"
    r:eval "exp5<-mean(exp[freqexpdis[4]+freqexpdis[3]+freqexpdis[2]+freqexpdis[1]+1:freqexpdis[5]])"
    r:eval "sum<-sum(exp1*fexp1,exp2*fexp2,exp3*fexp3,exp4*fexp4,exp5*fexp5)"
    r:eval "iexp1<-exp1*250000/sum"
    r:eval "iexp2<-exp2*250000/sum"
    r:eval "iexp3<-exp3*250000/sum"
    r:eval "iexp4<-exp4*250000/sum"
    r:eval "iexp5<-exp5*250000/sum"
    r:eval "valexpdis<-c(iexp1,iexp2,iexp3,iexp4,iexp5)"

    let fexp1.2 r:get "fexp1" 
    let fexp2.2 r:get "fexp2" 
    let fexp3.2 r:get "fexp3" 
    let fexp4.2 r:get "fexp4" 
    let fexp5.2 r:get "fexp5" 
   
      set fk1 r:get "fexp1"
      set fk2 r:get "fexp2"
      set fk3 r:get "fexp3"
      set fk4 r:get "fexp4"
      set fk5 r:get "fexp5"
       
    set k1 r:get "iexp1"
    set k2 r:get "iexp2"
    set k3 r:get "iexp3"
    set k4 r:get "iexp4"
    set k5 r:get "iexp5"
   
    ask n-of fexp1.2 patches with [k = 0] [set k k1]
    ask n-of fexp2.2 patches with [k = 0] [set k k2]
    ask n-of fexp3.2 patches with [k = 0] [set k k3]
    ask n-of fexp4.2 patches with [k = 0] [set k k4]
    ask n-of fexp5.2 patches with [k = 0] [set k k5]

    set sumk1 count patches with [k = k1]
    set sumk2 count patches with [k = k2]
    set sumk3 count patches with [k = k3]
    set sumk4 count patches with [k = k4]
    set sumk5 count patches with [k = k5]
  ]
    
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
214
10
974
791
-1
-1
15.0
1
18
1
1
1
0
1
1
1
0
49
0
49
1
1
1
ticks
30.0

BUTTON
9
40
72
73
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

BUTTON
144
40
207
73
NIL
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
77
40
140
73
step
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
980
274
1180
423
Resource
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"resource" 1.0 0 -10899396 true "" ""

PLOT
1185
274
1385
424
Movement
NIL
NIL
0.0
10.0
0.0
0.1
true
false
"" ""
PENS
"movement" 1.0 0 -7500403 true "" ""
"coopmovement" 1.0 0 -2674135 true "" ""
"selfimovement" 1.0 0 -955883 true "" ""

PLOT
981
426
1182
576
Harvesting
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"harvesting" 1.0 0 -16777216 true "" ""
"sustainable" 1.0 0 -16777216 true "" ""
"expected" 1.0 0 -16777216 true "" ""

PLOT
1186
578
1387
729
Storage
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"turtles" 1.0 0 -7500403 true "" ""
"selfish" 1.0 0 -2674135 true "" ""
"coop" 1.0 0 -13840069 true "" ""

PLOT
979
121
1179
271
Population
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"agents" 1.0 0 -16777216 true "" ""

SWITCH
1139
73
1229
106
plots?
plots?
1
1
-1000

SLIDER
25
382
197
415
num-cooperators
num-cooperators
0
10000
2500
1
1
NIL
HORIZONTAL

SLIDER
23
492
195
525
ia
ia
0
1
0.2
0.1
1
NIL
HORIZONTAL

CHOOSER
42
87
180
132
view
view
"resource" "population" "k"
0

SLIDER
24
455
196
488
move-capacity
move-capacity
0
25
25
1
1
NIL
HORIZONTAL

SLIDER
26
346
198
379
years-simulated
years-simulated
0
5000
5000
1
1
NIL
HORIZONTAL

PLOT
1186
425
1386
575
% Cheaters
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"agents" 1.0 0 -16777216 true "" ""
"selfish" 1.0 0 -2674135 true "" ""

CHOOSER
63
135
155
180
k?
k?
"default" "uniform" "normal" "exponential"
3

PLOT
8
185
208
335
k
NIL
NIL
0.0
150.0
0.0
2500.0
true
false
"" ""
PENS
"hist-k" 1.0 1 -16777216 true "" ""

PLOT
1184
121
1384
271
Settlements
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"set" 1.0 0 -16777216 true "" ""

SLIDER
23
528
195
561
pCatch
pCatch
0
1
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
25
417
197
450
num-selfish
num-selfish
0
5000
2500
1
1
NIL
HORIZONTAL

PLOT
983
580
1183
730
Type of agent
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"cooperator" 1.0 0 -13840069 true "" ""
"selfish" 1.0 0 -2674135 true "" ""

SLIDER
24
563
196
596
pMutate
pMutate
0
1
0.01
0.01
1
NIL
HORIZONTAL

SLIDER
24
598
196
631
pMove
pMove
0
1
0.5
0.1
1
NIL
HORIZONTAL

@#$#@#$#@
## ## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## ## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## ## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## ## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## ## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## ## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## ## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## ## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## ## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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
NetLogo 5.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="avg" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>avgset</metric>
    <metric>avgres</metric>
    <metric>avgpop</metric>
    <metric>avgmov</metric>
    <metric>avgcoop</metric>
    <metric>avgselfi</metric>
    <metric>avgcheat</metric>
    <metric>avgmovcoop</metric>
    <metric>avgmovselfi</metric>
    <metric>avgsumS</metric>
    <metric>avgsumScoop</metric>
    <metric>avgsumSselfi</metric>
    <metric>avgk1pop</metric>
    <metric>avgk2pop</metric>
    <metric>avgk3pop</metric>
    <metric>avgk4pop</metric>
    <metric>avgk5pop</metric>
    <metric>avgk1set</metric>
    <metric>avgk2set</metric>
    <metric>avgk3set</metric>
    <metric>avgk4set</metric>
    <metric>avgk5set</metric>
    <metric>avgk1res</metric>
    <metric>avgk2res</metric>
    <metric>avgk3res</metric>
    <metric>avgk4res</metric>
    <metric>avgk5res</metric>
    <metric>avgk1coop</metric>
    <metric>avgk2coop</metric>
    <metric>avgk3coop</metric>
    <metric>avgk4coop</metric>
    <metric>avgk5coop</metric>
    <metric>avgk1selfi</metric>
    <metric>avgk2selfi</metric>
    <metric>avgk3selfi</metric>
    <metric>avgk4selfi</metric>
    <metric>avgk5selfi</metric>
    <metric>avgk1cheat</metric>
    <metric>avgk2cheat</metric>
    <metric>avgk3cheat</metric>
    <metric>avgk4cheat</metric>
    <metric>avgk5cheat</metric>
    <enumeratedValueSet variable="move-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k?">
      <value value="&quot;default&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-cooperators">
      <value value="2500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-selfish">
      <value value="2500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="view">
      <value value="&quot;resource&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plots?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ia">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pCatch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="years-simulated">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pMutate">
      <value value="0.01"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sa" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>avgset</metric>
    <metric>avgres</metric>
    <metric>avgcoop</metric>
    <enumeratedValueSet variable="move-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k?">
      <value value="&quot;uniform&quot;"/>
      <value value="&quot;normal&quot;"/>
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-cooperators">
      <value value="2500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-selfish">
      <value value="2500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="view">
      <value value="&quot;resource&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plots?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ia">
      <value value="0.01"/>
      <value value="0.2"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pCatch">
      <value value="0.01"/>
      <value value="0.3"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="years-simulated">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pMutate">
      <value value="0.01"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="run" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>settlement</metric>
    <metric>sumres</metric>
    <metric>sumcoop</metric>
    <metric>sumpop</metric>
    <metric>sumselfi</metric>
    <enumeratedValueSet variable="move-capacity">
      <value value="1"/>
      <value value="5"/>
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k?">
      <value value="&quot;default&quot;"/>
      <value value="&quot;uniform&quot;"/>
      <value value="&quot;normal&quot;"/>
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-cooperators">
      <value value="2500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-selfish">
      <value value="2500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="view">
      <value value="&quot;resource&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plots?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ia">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pCatch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="years-simulated">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pMutate">
      <value value="0.01"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
