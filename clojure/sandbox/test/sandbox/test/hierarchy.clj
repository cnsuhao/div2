(ns sandbox.test.hierarchy
  (:use [clojure.test]))

;; basic shapes to test
(derive ::quad ::shape)
(derive ::rect ::quad)
(derive ::square ::rect)
(derive ::ellipsis ::shape)
(derive ::circle ::ellipsis)

(deftest basic-shapes
  (is (isa? ::rect ::quad) "Rectangle is a quad")
  (is (and (isa? ::square ::quad) (isa? ::square ::rect)) "Square is a rectangle and quad")
  (is (not (isa? ::circle ::rect)) "Circle is not a rectangle"))

;;;;;;;;;;;;;;;;;;
;; RPG elements 
;; 
;; An example of multiple inheritence
;;;;;;;;;;;;;;;;;;

;; basic
(derive ::sword ::weapon)
(derive ::axe ::weapon)
(derive ::spear ::weapon)

;; subtypes
(derive ::short-sword ::sword)
(derive ::short-sword ::one-handed)
(derive ::broad-sword ::sword)
(derive ::broad-sword ::two-handed)

(derive ::hand-axe ::axe)
(derive ::hand-axe ::one-handed)
(derive ::battle-axe ::axe)
(derive ::battle-axe ::two-handed)

(derive ::spear ::two-handed)

;; properties
(derive ::sword ::pointy)
(derive ::spear ::pointy)
(derive ::axe ::handled)
(derive ::spear ::handled)
(derive ::sword ::edged)
(derive ::axe ::edged)

(def weapons (list ::short-sword ::broad-sword ::hand-axe ::battle-axe ::spear ::sword ::axe ::weapon))

;; struct
(defstruct weapon :key :type :hands :pointy :handle :edged)

;; weapon type to struct
(defn make-weapon
  [typ]
  (struct-map weapon 
    :key typ
    :type (cond (isa? typ ::sword) ::sword
                (isa? typ ::axe) ::axe
                (isa? typ ::spear) ::spear)
    :hands (cond (isa? typ ::one-handed) 1
                 (isa? typ ::two-handed) 2)
    :pointy (isa? typ ::pointy)
    :handle (isa? typ ::handled)
    :edged  (isa? typ ::edged)))

;; struct to string
(defn pprint-weapon
	[{id :key , typ :type, hnd :hands, pnt :pointy, hdl :handle, edg :edged, :as weap}]
  (let [ not-nil? (comp not nil?)
         handed (if (not-nil? hnd) (format "%d handed" hnd) nil)
         pointy (if pnt "is pointy" nil)
         handle (if hdl "has handle" nil)
         edged  (if edg "has edge" nil)
         desc (apply str (interpose ", " (filter not-nil? (list handed pointy handle edged)))) ]
    (format "<%s> [%s]  %s" id (if (not-nil? typ) typ "unknown") (if (not-nil? typ) desc "no description") )))

;; actual test making sure the string output is what is expected
(deftest test-weapons
  (let [ allweapons {
				  ::weapon      (format "<%s> [unknown]  no description" ::weapon)
				  ::sword       (format "<%s> [%s]  is pointy, has edge" ::sword ::sword)
				  ::short-sword (format "<%s> [%s]  1 handed, is pointy, has edge" ::short-sword ::sword)
				  ::broad-sword (format "<%s> [%s]  2 handed, is pointy, has edge" ::broad-sword ::sword)
				  ::axe         (format "<%s> [%s]  has handle, has edge" ::axe ::axe)
				  ::hand-axe    (format "<%s> [%s]  1 handed, has handle, has edge" ::hand-axe ::axe)
				  ::battle-axe  (format "<%s> [%s]  2 handed, has handle, has edge" ::battle-axe ::axe)
				  ::spear       (format "<%s> [%s]  2 handed, is pointy, has handle" ::spear ::spear)
				}]
	  (doseq [weap (keys allweapons)]
	    (is (= (pprint-weapon (make-weapon weap)) (allweapons weap)) (format "Test output for %s" weap)))))