#lang racket/gui
(require plot)

(struct planet
  (Primaryidentifierofplanet
   Binaryflag
   Planetarymass
   Radius
   Period
   Semimajoraxis
   Eccentricity
   Periastron
   Longitude
   Ascendingnode
   Inclination
   Surfaceorequilibriumtemperature
   Age
   Discoverymethod
   Discoveryyear
   Lastupdated
   Rightascension
   Declination
   Distancefromsun
   Hoststarmass
   Hoststarradius
   Hoststarmetallicity
   Hoststartemperature
   Hoststarage))
(define (make-planet Primaryidentifierofplanet Binaryflag Planetarymass
                     Radius Period Semimajoraxis Eccentricity Periastron
                     Longitude Ascendingnode Inclination Surfaceorequilibriumtemperature
                     Age Discoverymethod Discoveryyear Lastupdated
                     Rightascension Declination Distancefromsun Hoststarmass
                     Hoststarradius Hoststarmetallicity Hoststartemperature
                     Hoststarage)

  (define (string->string-or-false str)
    (if (string=? str "") #f str))
  (planet (string->string-or-false  Primaryidentifierofplanet)
        (string->number Binaryflag)
        (string->number Planetarymass)
        (string->number Radius)
        (string->number Period)
        (string->number Semimajoraxis)
        (string->number Eccentricity)
        (string->number Periastron)
        (string->number Longitude)
        (string->number Ascendingnode)
        (string->number Inclination)
        (string->number Surfaceorequilibriumtemperature)
        (string->number Age)
        (string->string-or-false Discoverymethod)
        (string->number Discoveryyear)
        (string->number Lastupdated)
        (string->number Rightascension)
        (string->number Declination)
        (string->number Distancefromsun)
        (string->number Hoststarmass)
        (string->number Hoststarradius)
        (string->number Hoststarmetallicity)
        (string->number Hoststartemperature)
        (string->number Hoststarage)))

  (define port (open-input-file "open_exoplanet_catalogue.txt" #:mode 'text))
  (define planet-list
    (begin
      ;;throw away first 30 lines
      (for([i 30])
        (read-line port))
      (for/fold ((planet-list null))
                ((line (in-lines port)))
        (define fields (regexp-split #rx"," line))
        (cond ((= (length fields) 24)
               (define planet (apply make-planet fields))
               (cons planet planet-list))
              (else
              planet-list)))))

(close-input-port port)

;;counter for the number of binary planets
(define bpcounter 0)
;;increments the counter
(define (inc)
  (set! bpcounter (add1 bpcounter)))

;;number of unconfirmed planets
(define numunconfirm 0)
;;increments the counter
(define (incconfirm)
  (set! numunconfirm (add1 numunconfirm)))

;;counter number of binary and unconfirmed planets
(for/list ((planet (in-list planet-list)))
  (cond ((= (planet-Binaryflag planet) 2)
         (inc)))
  (cond ((eq? (planet-Discoverymethod planet) #f)
         (incconfirm))))

;;list of primary identifiers of planets with last char removed
(define mylist '())
(for/list ((planet (in-list planet-list)))  
  (set! mylist(cons (substring (planet-Primaryidentifierofplanet planet) 0 ( - (string-length (planet-Primaryidentifierofplanet planet)) 1))  mylist)))

;;set up for first graph

;;list of Radial Velocity Semimajoraxis of planets
(define RVsmlist '())
;;list of Radial Velocity planetarymass of planets
(define RVpmasslist'())
;;list of transit Semimajoraxis of planets
(define tsmlist '())
;;list of transit planetarymass of planets
(define tpmasslist'())
;;list of microlensing  Semimajoraxis of planets
(define mlsmlist '())
;;list of microlensing planetarymass of planets
(define mlpmasslist'())
;;list of imaging  Semimajoraxis of planets
(define ismlist '())
;;list of imaging planetarymass of planets
(define ipmasslist'())
;;only adds to lists if both semimajor axis and planetarymass of planet is valid(not empty) for the purpose of graphing them
(for/list ((planet (in-list planet-list)))
  (cond ((not(eq? (planet-Semimajoraxis planet) #f))
         (cond((not(eq? (planet-Planetarymass planet) #f))
              (cond
                [(equal? (planet-Discoverymethod planet) "RV") 
               (set! RVsmlist(cons(planet-Semimajoraxis planet) RVsmlist))(set! RVpmasslist(cons(planet-Planetarymass planet) RVpmasslist))]
                [(equal? (planet-Discoverymethod planet) "transit") 
               (set! tsmlist(cons(planet-Semimajoraxis planet) tsmlist))(set! tpmasslist(cons(planet-Planetarymass planet) tpmasslist))]
                [(equal? (planet-Discoverymethod planet) "microlensing") 
               (set! mlsmlist(cons(planet-Semimajoraxis planet) mlsmlist))(set! mlpmasslist(cons(planet-Planetarymass planet) mlpmasslist))]
                [(equal? (planet-Discoverymethod planet) "imaging") 
               (set! ismlist(cons(planet-Semimajoraxis planet) ismlist))(set! ipmasslist(cons(planet-Planetarymass planet) ipmasslist))]))))))

;;Setting up lists for second graph

;;list of Semimajoraxis of planets
(define smlist '())
;;list of planetarymass of planets
(define pmasslist'())
;;only adds to lists if both semimajor axis and planetarymass of planet is valid(not empty) for the purpose of graphing them
(for/list ((planet (in-list planet-list)))
  (cond ((not(eq? (planet-Semimajoraxis planet) #f))
         (cond((not(eq? (planet-Planetarymass planet) #f))
               (set! smlist(cons(planet-Semimajoraxis planet) smlist))(set! pmasslist(cons(planet-Planetarymass planet) pmasslist)))))))
;;first graph: Semimajoraxis vs Planetarymass (both x and y axis increment logarithmically) and the ticks are scaled so they are the same size
(parameterize ([plot-width    420]
                 [plot-height   350]
                 [plot-x-label  "Semi-major axis[AU]"]
                 [plot-y-label  "Planet Mass [MJupiter]"]
                 [plot-x-transform  log-transform]
                 [plot-x-ticks      (log-ticks)]
                 [plot-y-transform  log-transform]
                 [plot-y-ticks      (log-ticks)])
  (plot (list(points (map vector RVpmasslist RVsmlist )  #:color 'darkgreen #:sym 'fullcircle2 #:label "Radial Velocity Planets")
             (points (map vector tpmasslist tsmlist )  #:color 'blue #:sym 'fullcircle2 #:label "Transiting Planets")
             (points (map vector ipmasslist ismlist )  #:color 'red #:sym 'fullcircle2 #:label "Directly imaged Planets")
          (points (map vector mlpmasslist mlsmlist )
                        #:x-min 0.001 #:x-max 50
                        #:y-min 0.008 #:y-max 350 #:color 'cyan #:sym 'fullcircle2 #:label "Microlensing Planets"))))

;;second graph: Semimajoraxis vs Planetarymass (both x and y axis increment logarithmically) and the ticks are scaled so they are the same size
(parameterize ([plot-width    460]
                 [plot-height   460]
                 [plot-x-transform  log-transform]
                ;; [plot-x-ticks      (log-ticks)]
                 )
  (plot (list(points (map vector pmasslist smlist )
                        #:x-min 0.003 #:x-max 10
                        #:y-min -1  #:y-max 10 #:color 'darkgreen #:sym 'fullcircle2 )(point-label  smlist))))

;;take length of planet-list to get total number of planets
(printf "The total number of planets (including Solar System objects and unconfirmed exoplanets) is: ~a. \n"
        (length planet-list))
;;counter keeps track of the number of binary planets
(printf "The total number of Binary Systems is: ~a.\n" bpcounter)
(printf "The number of confirmed exoplantes is: ~a.\n" (- (length planet-list) numunconfirm))
(printf "The total number of Planetary Systems is: ~a.\n" (length (remove-duplicates mylist)))