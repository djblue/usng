(ns usng.core
  (:refer-clojure :exclude [replace repeat])
  (:require [clojure.string :as str])
  #?(:clj
     (:import (org.codice.usng4j BoundingBox UsngCoordinate UtmCoordinate CoordinatePrecision DecimalDegreesCoordinate))))

(def DEG_2_RAD (/ Math/PI 180))
(def RAD_2_DEG (/ 180 Math/PI))
(def BLOCK_SIZE 100000)
(def GRIDSQUARE_SET_COL_SIZE 8)
(def GRIDSQUARE_SET_ROW_SIZE 20)
(def EASTING_OFFSET 500000)
(def NORTHING_OFFSET 10000000)
(def k0 0.9996)
(def USNGSqLetOdd "ABCDEFGHJKLMNPQRSTUV")
(def USNGSqLetEven "FGHJKLMNPQRSTUVABCDE")
(def EQUATORIAL_RADIUS 6378206.4)
(def ECC_SQUARED 0.006768658)
(def NAD83_EQUATORIAL_RADIUS 6378137)
(def NAD83_ECC_SQUARED 0.006694380023)

#?(:cljs
   (do
     (def = identical?)

     (defn str
       ([a] (.join #js [a] ""))
       ([a b] (.join #js [a b] ""))
       ([a b c] (.join #js [a b c] ""))
       ([a b c d] (.join #js [a b c d] ""))
       ([a b c d e f g] (.join #js [a b c d e f g] "")))

     (defn even? [n]
       (= (mod n 2) 0))

     (defn nth [coll index]
       (cond
         (string? coll)
         (.charAt coll index))
       :else (aget coll index))

     (defn count [coll]
       (.-length coll))))

(def parseFloat
  #?(:cljs js/Number.parseFloat :clj #(identity %)))

(def parseInt
  #?(:cljs js/Number.parseInt :clj #(.intValue %)))

(def isNaN
  #?(:cljs js/Number.isNaN :clj (fn [_] false)))

(def isFinite
  #?(:cljs js/Number.isFinite :clj #(not (.isInfinite ^Float %))))

(defn toFixed [n p] (.toFixed n p))

(defn replace [s match replacement]
  #?(:clj  (str/replace s match replacement)
     :cljs (.replace s (js/RegExp match "g") replacement)))

(defn find-index [coll fn]
  (loop [i 0]
    (if-let [v (nth coll i)]
      (if (fn v) i (recur (inc i))) -1)))

(defn repeat [s n]
  #?(:cljs (.repeat s n)
     :clj  (apply str (clojure.core/repeat n s))))

(defn error [^String msg]
  #?(:cljs (js/Error. msg) :clj (Exception. msg)))

(defn create-dd [lat lon]
  #?(:cljs #js {"lat" lat "lon" lon}
     :clj  (reify DecimalDegreesCoordinate
             (getLat [this] lat)
             (getLon [this] lon))))

(defn dd-get-lat [^DecimalDegreesCoordinate dd]
  #?(:cljs (.-lat dd) :clj (.getLat dd)))

(defn dd-get-lon [^DecimalDegreesCoordinate dd]
  #?(:cljs (.-lon dd) :clj (.getLon dd)))

(defn create-bbox [n e s w]
  #?(:cljs #js {"north" n "east" e "south" s "west" w}
     :clj  (reify BoundingBox
             (getNorth [this] n)
             (getSouth [this] s)
             (getEast [this] e)
             (getWest [this] w))))

(defn bbox-get-north [^BoundingBox bb]
  #?(:cljs (.-north bb) :clj (.getNorth bb)))

(defn bbox-get-east [^BoundingBox bb]
  #?(:cljs (.-east bb) :clj (.getEast bb)))

(defn bbox-get-south [^BoundingBox bb]
  #?(:cljs (.-south bb) :clj (.getSouth bb)))

(defn bbox-get-west [^BoundingBox bb]
  #?(:cljs (.-west bb) :clj (.getWest bb)))

(defn create-utm
  ([easting northing zone] (create-utm easting northing zone nil))
  ([easting northing zone letter]
   #?(:cljs #js {"E" easting "N" northing "zone" zone "letter" letter}
      :clj  (reify UtmCoordinate
                (getEasting [this] easting)
                (getNorthing [this] northing)
                (getZoneNumber [this] zone)
                (getLattitudeBand [this] letter)
                (getPrecision [this])))))

(defn utm-get-easting [^UtmCoordinate utm]
  #?(:cljs (.-E utm) :clj (.getEasting utm)))

(defn utm-get-northing [^UtmCoordinate utm]
  #?(:cljs (.-N utm) :clj (.getNorthing utm)))

(defn utm-get-zone [^UtmCoordinate utm]
  #?(:cljs (.-zone utm) :clj (.getZoneNumber utm)))

(defn utm-get-lattitude-band [^UtmCoordinate utm]
  #?(:cljs (.-letter utm) :clj (.getLattitudeBand utm)))

(defn utm-get-precision [^UtmCoordinate utm]
  #?(:cljs nil :clj (.getPrecision utm)))

(declare usng->str)

(defn create-usng [zone letter sq1 sq2 precision easting northing]
  #?(:cljs #js {"zone"      zone
                "let"       letter
                "sq1"       sq1
                "sq2"       sq2
                "precision" precision
                "east"      easting
                "north"     northing}
     :clj  (reify UsngCoordinate
             (getZoneNumber [this] zone)
             (getLatitudeBandLetter [this] (.charAt letter 0))
             (getColumnLetter [this] sq1)
             (getRowLetter [this] sq2)
             (getEasting [this]
               (let [digitPrecision 0
                       digitPrecision (if (> precision 0) (dec precision) digitPrecision)
                       digitPrecision (if (> digitPrecision 5) 5 digitPrecision)]
                   (if (and (>= digitPrecision 1) (some? easting))
                     (.intValue easting))))
             (getNorthing [this]
               (let [digitPrecision 0
                     digitPrecision (if (> precision 0) (dec precision) digitPrecision)
                     digitPrecision (if (> digitPrecision 5) 5 digitPrecision)]
                 (if (and (>= digitPrecision 1) (some? northing))
                   (.intValue northing))))
             (getPrecision [this]
               (case precision
                 0 CoordinatePrecision/SIX_BY_EIGHT_DEGREES
                 1 CoordinatePrecision/ONE_HUNDRED_KILOMETERS
                 2 CoordinatePrecision/TEN_KILOMETERS
                 3 CoordinatePrecision/ONE_KILOMETER
                 4 CoordinatePrecision/ONE_HUNDRED_METERS
                 5 CoordinatePrecision/TEN_METERS
                 6 CoordinatePrecision/ONE_METER))
             (toMgrsString [this])
             Object
             (equals [this b] true)
             (toString [this] (usng->str this)))))

(defn usng-get-zone [^UsngCoordinate usng]
  #?(:cljs (.-zone usng) :clj (.getZoneNumber usng)))

(defn usng-get-letter [^UsngCoordinate usng]
  #?(:cljs (.-let usng) :clj (.getLatitudeBandLetter usng)))

(defn usng-get-column-letter [^UsngCoordinate usng]
  #?(:cljs (.-sq1 usng) :clj (.getColumnLetter usng)))

(defn usng-get-row-letter [^UsngCoordinate usng]
  #?(:cljs (.-sq2 usng) :clj (.getRowLetter usng)))

(defn usng-get-easting [^UsngCoordinate usng]
  #?(:cljs (.-east usng) :clj (.getEasting usng)))

(defn usng-get-northing [^UsngCoordinate usng]
  #?(:cljs (.-north usng) :clj (.getNorthing usng)))

(defn usng-get-precision [^UsngCoordinate usng]
  #?(:cljs (.-precision usng) :clj (min 5 (inc (.getIntValue (.getPrecision usng))))))

(defn getZoneNumber [lat lon]
  (let [lat (parseFloat lat)
        lon (parseFloat lon)]
    (when (or (or (or (> lon 360) (< lon -180)) (> lat 84)) (< lat -80))
      (throw (error (str "usng.js, getZoneNumber: invalid input. lat: " (toFixed lat 4) " lon: " (toFixed lon 4)))))
    (let [lonTemp (- (- (+ lon 180) (* (parseInt (/ (+ lon 180) 360)) 360)) 180)
          zoneNumber (inc (parseInt (/ (+ lonTemp 180) 6)))
          zoneNumber (if (and (and (and (>= lat 56) (< lat 64)) (>= lonTemp 3)) (< lonTemp 12)) 32 zoneNumber)]
      (cond
        (and (>= lat 72) (< lat 84)) zoneNumber
        (and (>= lonTemp 0) (< lonTemp 9)) 31
        (and (>= lonTemp 9) (< lonTemp 21)) 33
        (and (>= lonTemp 21) (< lonTemp 33)) 35
        (and (>= lonTemp 33) (< lonTemp 42)) 37
        true zoneNumber))))

(defn LLtoKM [lat1 lon1 lat2 lon2]
  (let [R 6371000
        phi1 (* lat1 DEG_2_RAD)
        phi2 (* lat2 DEG_2_RAD)
        deltaPhi (* (- lat2 lat1) DEG_2_RAD)
        deltaLlamda (* (- lon2 lon1) DEG_2_RAD)
        a (+ (* (Math/sin (/ deltaPhi 2)) (Math/sin (/ deltaPhi 2))) (* (* (* (Math/cos phi1) (Math/cos phi2)) (Math/sin (/ deltaLlamda 2))) (Math/sin (/ deltaLlamda 2))))
        c (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a))))]
    (* R c)))

(defn UTMLetterDesignator [lat]
  (let [lat (parseFloat lat)]
    (if (or (> lat 84) (< lat -80))
      "Z"
      (let [index (/ (+ lat 80) 8)
            index (if (>= index 6) (inc index) index)
            index (if (>= index 12) (inc index) index)
            index (if (>= index 22) (dec index) index)]
        (str (char (+ 67 index)))))))

(defn dd->utm
  ([isNad83? dd] (dd->utm isNad83? dd nil))
  ([isNad83? dd zone]
   (let [squared (if isNad83? NAD83_ECC_SQUARED ECC_SQUARED)
         radius (if isNad83? NAD83_EQUATORIAL_RADIUS EQUATORIAL_RADIUS)
         primeSquared (/ squared (- 1 squared))
         lat (dd-get-lat dd)
         lon (dd-get-lon dd)
         lat (parseFloat lat)
         lon (parseFloat lon)]
     (when-not (or (> lat 84) (< lat -80))
       (when (or (or (or (> lon 360) (< lon -180)) (> lat 90)) (< lat -90))
         (throw (error (str "usng.js, LLtoUTM, invalid input. lat: " (toFixed lat 4) " lon: " (toFixed lon 4)))))
       (let [lonTemp (- (- (+ lon 180) (* (parseInt (/ (+ lon 180) 360)) 360)) 180)
             latRad (* lat DEG_2_RAD)
             lonRad (* lonTemp DEG_2_RAD)
             zoneNumber (if (not zone) (getZoneNumber lat lon) zone)
             lonOrigin (+ (- (* (dec zoneNumber) 6) 180) 3)
             lonOriginRad (* lonOrigin DEG_2_RAD)
             UTMZone (str zoneNumber (UTMLetterDesignator lat) " ")
             N (/ radius (Math/sqrt (- 1 (* (* squared (Math/sin latRad)) (Math/sin latRad)))))
             T (* (Math/tan latRad) (Math/tan latRad))
             C (* (Math/pow (Math/cos latRad) 2) primeSquared)
             A (* (Math/cos latRad) (- lonRad lonOriginRad))
             M (* radius (- (+ (- (* (- 1 (/ squared 4) (/ (* 3 squared squared) 64) (/ (* 5 squared squared squared) 256)) latRad) (* (+ (+ (/ (* 3 squared) 8) (/ (* 3 squared squared) 32)) (/ (* 45 squared squared squared) 1024)) (Math/sin (* 2 latRad)))) (* (+ (/ (* (* 15 squared) squared) 256) (/ (* 45 squared squared squared) 1024)) (Math/sin (* 4 latRad)))) (* (/ (* 35 squared squared squared) 3072) (Math/sin (* 6 latRad)))))
             easting (+ (* k0 N (+ A (/ (* (+ (- 1 T) C) (* A A A)) 6) (/ (* (- (+ (- 5 (* 18 T)) (* T T) (* 72 C)) (* 58 primeSquared)) (* A A A A A)) 120))) EASTING_OFFSET)
             northing (* k0 (+ M (* N (Math/tan latRad) (+ (/ (* A A) 2) (/ (* (+ (- 5 T) (* 9 C) (* 4 C C)) (* A A A A)) 24) (/ (* (- (+ (- 61 (* 58 T)) (* T T) (* 600 C)) (* 330 primeSquared)) (* A A A A A A)) 720)))))]
         (create-utm easting northing zoneNumber))))))

(defn findSet [zoneNum]
  (let [v (mod (parseInt zoneNum) 6)]
    (cond
      (= v 0) 6
      (= v 1) 1
      (= v 2) 2
      (= v 3) 3
      (= v 4) 4
      (= v 5) 5
      true -1)))

(defn lettersHelper [setter row col]
  (let [row (dec (if (zero? row) GRIDSQUARE_SET_ROW_SIZE row))
        col (dec (if (zero? col) GRIDSQUARE_SET_COL_SIZE col))]
    (cond
      (= setter 1) (str (nth "ABCDEFGH" col) (nth USNGSqLetOdd row))
      (= setter 2) (str (nth "JKLMNPQR" col) (nth USNGSqLetEven row))
      (= setter 3) (str (nth "STUVWXYZ" col) (nth USNGSqLetOdd row))
      (= setter 4) (str (nth "ABCDEFGH" col) (nth USNGSqLetEven row))
      (= setter 5) (str (nth "JKLMNPQR" col) (nth USNGSqLetOdd row))
      (= setter 6) (str (nth "STUVWXYZ" col) (nth USNGSqLetEven row)))))

(defn squareIdentifier [row north_1m]
  (if (< north_1m BLOCK_SIZE)
    row
    (squareIdentifier (inc row) (- north_1m BLOCK_SIZE))))

(defn findGridLetters [zoneNum northing easting]
  (let [row (mod (squareIdentifier 1 (Math/round (parseFloat northing))) GRIDSQUARE_SET_ROW_SIZE)
        col (mod (squareIdentifier 0 (Math/round (parseFloat easting))) GRIDSQUARE_SET_COL_SIZE)]
    (lettersHelper (findSet (parseInt zoneNum)) row col)))

(defn dd->usng [isNad83? dd precision]
  (let [lat (dd-get-lat dd)
        lon (dd-get-lon dd)
        lon (if (< lon -180) (+ lon 360) (if (> lon 180) (- lon 360) lon))
        coords (dd->utm isNad83? dd)
        UTMEasting (utm-get-easting coords)
        UTMNorthing (utm-get-northing coords)
        UTMNorthing (if (< lat 0) (+ UTMNorthing NORTHING_OFFSET) UTMNorthing)
        zoneNumber (getZoneNumber lat lon)
        USNGLetters (findGridLetters zoneNumber UTMNorthing UTMEasting)
        USNGNorthing (mod (Math/round UTMNorthing) BLOCK_SIZE)
        USNGEasting (mod (Math/round UTMEasting) BLOCK_SIZE)
        precision (if (or (nil? precision) (< precision 0)) 0 precision)
        digitPrecision 0
        digitPrecision (if (> precision 0) (dec precision) digitPrecision)
        digitPrecision (if (> digitPrecision 5) 5 digitPrecision)
        USNGNorthing (Math/floor (/ USNGNorthing (Math/pow 10 (- 5 digitPrecision))))
        USNGEasting (Math/floor (/ USNGEasting (Math/pow 10 (- 5 digitPrecision))))]
    (create-usng zoneNumber
                 (UTMLetterDesignator lat)
                 (if (>= precision 1) (nth USNGLetters 0))
                 (if (>= precision 1) (nth USNGLetters 1))
                 precision
                 USNGEasting
                 USNGNorthing)))

(defn usng->str [usng]
  (let [zone (usng-get-zone usng)
        easting (usng-get-easting usng)
        northing (usng-get-northing usng)
        precision (usng-get-precision usng)
        letter (usng-get-letter usng)
        sq1 (usng-get-column-letter usng)
        sq2 (usng-get-row-letter usng)
        digitPrecision 0
        digitPrecision (if (> precision 0) (dec precision) digitPrecision)
        digitPrecision (if (> digitPrecision 5) 5 digitPrecision)]
    (str
      zone
      letter
      (if (>= precision 1) (str " " sq1 sq2) "")
      (if (>= digitPrecision 1)
        (str ""
             " "
             (repeat "0"
                     (- digitPrecision (count (str easting))))
             easting
             " "
             (repeat "0"
                     (- digitPrecision (count (str northing))))
             northing)))))

(defn bbox->usng [isNad83? bb]
  (let [north (bbox-get-north bb)
        south (bbox-get-south bb)
        east (bbox-get-east bb)
        west (bbox-get-west bb)
        northNum (parseFloat north)
        southNum (parseFloat south)
        eastNum (parseFloat east)
        westNum (parseFloat west)
        lat (/ (+ northNum southNum) 2)
        lon (/ (+ eastNum westNum) 2)
        lat (if (>= lat 90) 89.9 (if (<= lat -90) -89.9 lat))
        lon (if (>= lon 180) 179.9 (if (<= lon -180) -179.9 lon))
        R 6371000
        phi1 (* northNum DEG_2_RAD)
        phi2 (* southNum DEG_2_RAD)
        deltaPhi (* (- southNum northNum) DEG_2_RAD)
        deltaLlamda (* (- westNum eastNum) DEG_2_RAD)
        height (* (Math/sin (/ deltaPhi 2)) (Math/sin (/ deltaPhi 2)))
        height (* (* R 2) (Math/atan2 (Math/sqrt height) (Math/sqrt (- 1 height))))
        length (* (* (* (Math/cos phi1) (Math/cos phi2)) (Math/sin (/ deltaLlamda 2))) (Math/sin (/ deltaLlamda 2)))
        length (* (* R 2) (Math/atan2 (Math/sqrt length) (Math/sqrt (- 1 length))))
        dist (max height length)
        lon (if (and (and (zero? lon) (or (> eastNum 90) (< eastNum -90))) (or (> westNum 90) (< westNum -90))) 180 lon)
        dd (create-dd lat lon)]
    (dd->usng isNad83?
              dd
              (cond
                (> dist 100000) 0
                (> dist 10000) 1
                (> dist 1000) 2
                (> dist 100) 3
                (> dist 10) 4
                (> dist 1) 5
                (>= dist 0) 6))))

(defn utm->dd [isNad83? utm]
  (let [UTMNorthing (utm-get-northing utm)
        UTMEasting (utm-get-easting utm)
        UTMZoneNumber (utm-get-zone utm)
        radius (if isNad83? NAD83_EQUATORIAL_RADIUS EQUATORIAL_RADIUS)
        squared (if isNad83? NAD83_ECC_SQUARED ECC_SQUARED)
        primeSquared (/ squared (- 1 squared))
        e1 (/ (- 1 (Math/sqrt (- 1 squared))) (+ 1 (Math/sqrt (- 1 squared))))
        xUTM (- (parseFloat UTMEasting) EASTING_OFFSET)
        yUTM (parseFloat UTMNorthing)
        zoneNumber (parseInt UTMZoneNumber)
        lonOrigin (+ (- (* (dec zoneNumber) 6) 180) 3)
        M (/ yUTM k0)
        mu (/ M (* radius (- (- (- 1 (/ squared 4)) (/ (* (* 3 squared) squared) 64)) (/ (* (* (* 5 squared) squared) squared) 256))))
        phi1Rad (+ (+ (+ mu (* (- (/ (* 3 e1) 2) (/ (* (* (* 27 e1) e1) e1) 32)) (Math/sin (* 2 mu)))) (* (- (/ (* (* 21 e1) e1) 16) (/ (* (* (* (* 55 e1) e1) e1) e1) 32)) (Math/sin (* 4 mu)))) (* (/ (* (* (* 151 e1) e1) e1) 96) (Math/sin (* 6 mu))))
        phi1 (* phi1Rad RAD_2_DEG)
        N1 (/ radius (Math/sqrt (- 1 (* (* squared (Math/sin phi1Rad)) (Math/sin phi1Rad)))))
        T1 (* (Math/tan phi1Rad) (Math/tan phi1Rad))
        C1 (* (* primeSquared (Math/cos phi1Rad)) (Math/cos phi1Rad))
        R1 (/ (* radius (- 1 squared)) (Math/pow (- 1 (* (* squared (Math/sin phi1Rad)) (Math/sin phi1Rad))) 1.5))
        D (/ xUTM (* N1 k0))
        lat (- phi1Rad (* (/ (* N1 (Math/tan phi1Rad)) R1) (+ (- (/ (* D D) 2) (/ (* (* (* (* (- (- (+ (+ 5 (* 3 T1)) (* 10 C1)) (* (* 4 C1) C1)) (* 9 primeSquared)) D) D) D) D) 24)) (/ (* (* (* (* (* (* (- (- (+ (+ (+ 61 (* 90 T1)) (* 298 C1)) (* (* 45 T1) T1)) (* 252 primeSquared)) (* (* 3 C1) C1)) D) D) D) D) D) D) 720))))
        lat (* lat RAD_2_DEG)
        lat (if (zero? lat) 0.001 lat)
        lon (/ (+ (- D (/ (* (* (* (+ (+ 1 (* 2 T1)) C1) D) D) D) 6)) (/ (* (* (* (* (* (+ (+ (- (+ (- 5 (* 2 C1)) (* 28 T1)) (* (* 3 C1) C1)) (* 8 primeSquared)) (* (* 24 T1) T1)) D) D) D) D) D) 120)) (Math/cos phi1Rad))
        lon (+ lonOrigin (* lon RAD_2_DEG))]
    (create-dd lat lon)))

(defn zone-letter-lats [letter fn]
  (cond
    (= letter "C") (fn -80 -72)
    (= letter "D") (fn -72 -64)
    (= letter "E") (fn -64 -56)
    (= letter "F") (fn -56 -48)
    (= letter "G") (fn -48 -40)
    (= letter "H") (fn -40 -32)
    (= letter "J") (fn -32 -24)
    (= letter "K") (fn -24 -16)
    (= letter "L") (fn -16 -8)
    (= letter "M") (fn -8 -0.01)
    (= letter "N") (fn 0.01 8)
    (= letter "P") (fn 8 16)
    (= letter "Q") (fn 16 24)
    (= letter "R") (fn 24 32)
    (= letter "S") (fn 32 40)
    (= letter "T") (fn 40 48)
    (= letter "U") (fn 48 56)
    (= letter "V") (fn 56 64)
    (= letter "W") (fn 64 72)
    (= letter "X") (fn 72 84)))

(defn utm->bbox [isNad83? utm accuracy]
  (let [northing (utm-get-northing utm)
        easting (utm-get-easting utm)
        zone (utm-get-zone utm)
        dd (utm->dd isNad83? utm)
        lat (dd-get-lat dd)
        lon (dd-get-lon dd)]
    (if (<= accuracy 100000)
      (let [utm (create-utm (+ easting accuracy) (+ northing accuracy) zone)
            north-east (utm->dd isNad83? utm)]
        (create-bbox (dd-get-lat north-east) (dd-get-lon north-east) lat lon))
      (let [zoneLetter (UTMLetterDesignator lat)
            east (+ -180 (* 6 zone))
            west (- east 6)]
        (zone-letter-lats
          zoneLetter
          (fn [south north]
            (create-bbox north east south west)))))))

(defn scale [n] (* n 1000000))

(def zoneBase
  #?(:cljs (.map #js [1.1 2 2.8 3.7 4.6 5.5 6.4 7.3 8.2 9.1 0 0.8 1.7 2.6 3.5 4.4 5.3 6.2 7 7.9] scale)
     :clj  (map scale [1.1 2 2.8 3.7 4.6 5.5 6.4 7.3 8.2 9.1 0 0.8 1.7 2.6 3.5 4.4 5.3 6.2 7 7.9])))

(defn totalNorthing [northing letter]
  (if (< northing (nth zoneBase (str/index-of "CDEFGHJKLMNPQRSTUVWX" letter)))
    (totalNorthing (+ northing 2000000) letter) northing))

(defn getNorthing [zone letter sq2 north precision]
  (if-not (#?(:cljs undefined? :clj nil?)  sq2)
    (let [northingArray (if (even? zone) "FGHJKLMNPQRSTUVABCDE" "ABCDEFGHJKLMNPQRSTUV")
          northing (totalNorthing (* (str/index-of northingArray sq2) 100000) letter)]
      (+ northing (if-not (isNaN north)
                    (* north
                       (Math/pow 10 (- 5 precision)))
                    0)))
    (+ (nth zoneBase (str/index-of "CDEFGHJKLMNPQRSTUVWX" letter)) 499600)))

(def eastingArray
  #?(:cljs #js ["" "AJS" "BKT" "CLU" "DMV" "ENW" "FPX" "GQY" "HRZ"]
     :clj  ["" "AJS" "BKT" "CLU" "DMV" "ENW" "FPX" "GQY" "HRZ"]))

(defn getEasting [sq1 east precision]
  (let [i (find-index eastingArray (fn [v] (str/includes? v (str sq1))))
        easting (* i 100000)]
    (if-not (isNaN east)
      (+ easting (* east (Math/pow 10 (- 5 precision))))
      easting)))

(defn usng->utm [usng]
  (let [zone (usng-get-zone usng)
        letter (usng-get-letter usng)
        sq1 (usng-get-column-letter usng)
        sq2 (usng-get-row-letter usng)
        east (usng-get-easting usng)
        north (usng-get-northing usng)
        precision (usng-get-precision usng)
        easting (getEasting sq1 east precision)
        northing (getNorthing zone letter sq2 north precision)]
    (create-utm easting northing zone letter)))

(defn str->usng [input]
  (if (nil? input)
    0
    (let [input (replace (replace (str/upper-case input) #"%20" "") #" " "")]
      (if (< (count input) 2)
        0
        (let [n (parseFloat (nth input 1))
              is2DigitZone (and (not (isNaN n)) (isFinite n))
              zone (parseInt (subs input 0 (if is2DigitZone 2 1)))
              j (if is2DigitZone 2 1)
              _let (nth input j)
              sq1 (nth input (+ j 1))
              sq2 (nth input (+ j 2))
              j (+ j 3)
              precision (/ (- (count input) j) 2)
              east (subs input j (+ j precision))
              j (+ j precision)
              j (if (= (nth input j) " ") (inc j) j)
              north (subs input j (+ j precision))]
          (create-usng zone _let sq1 sq2 precision east north))))))

(defn char< [a b]
  #?(:cljs (< a b) :clj (< (int a) (int b))))

(defn usng->dd [is-nad83? usng get-center?]
  (let [utm (usng->utm usng)
        accuracy (if (not get-center?) (/ 100000 (Math/pow 10 (usng-get-precision usng))) nil)
        northing (utm-get-northing utm)
        northing (if (char< (usng-get-letter usng) (nth "N" 0))
                   (- northing NORTHING_OFFSET) northing)
        utm (create-utm (utm-get-easting utm) northing (usng-get-zone usng))]
    (if (some? accuracy)
      (utm->bbox is-nad83? utm accuracy)
      (utm->dd is-nad83? utm))))
