(ns usng.test
  (:require [clojure.test :refer [deftest is]]
            [usng.core :as usng]))

(def zone-numbers-test-cases
  [[12 [34 -111] "around Arizona in the United States"]
   [12 [34.5 -112.5] "around Prescott/Chino Valley in Arizona"]
   [12 [34.545 -112.465] "immediately around Prescott city in Arizona"]
   [21 [-32.5 -55.5] "around Uruguay"]
   [21 [-34.5 -58.5] "around Buenos Aires city in Argentina"]
   [21 [-34.66 -58.73] "around Merlo town in Buenos Aires"]
   [38 [-18.5 46.5] "around Madagascar"]
   [38 [-22.5 43.5] "around Toliara city in Madagascar"]
   [38 [-23.355 43.67] "around Toliara city center in Madagascar"]
   [54 [37 140.5] "around Central Japan"]
   [54 [35.5 139.5] "around Tokyo city in Japan"]
   [54 [35.69 139.77] "around Tokyo city center in Japan"]
   ; around the international date line
   [60 [28 179] "to the immediate west"]
   [1 [28 -179] "to the immediate east"]
   [1 [28 -180] "with midpoint directly on it [-180]"]
   [1 [28 180] "with midpoint directly on it [+180]"]
   ; around the equator
   [54 [1 141] "to the immediate north"]
   [54 [-1 141] "to the immediate south"]
   [54 [0 141] "with midpoint directly on it"]
   ; around the international date line and equator
   [60 [1 179] "to the immediate west and north"]
   [60 [-1 179] "to the immediate west and south"]
   [1 [1 -179] "to the immediate east and north"]
   [1 [-1 -179] "to the immediate east and south"]
   [1 [0 -180] "with midpoint directly on it [0 -180]"]
   [1 [0 180] "with midpoint directly on it [0 +180]"]])

(deftest zone-number
  (doseq [[result args] zone-numbers-test-cases]
    (is (= (apply usng/getZoneNumber args) result))))

(def utm-letter-designator-test-cases
  [; Get Zone letter from lat
   ["S" [34] "around Arizona in the United States"]
   ["S" [34.5] "around Prescott/Chino Valley in Arizona"]
   ["S" [34.545] "immediately around Prescott city in Arizona"]
   ["H" [-32.5] "around Uruguay"]
   ["H" [-34.5] "around Buenos Aires city in Argentina"]
   ["H" [-34.66] "around Merlo town in Buenos Aires"]
   ["K" [-18.5] "around Madagascar"]
   ["K" [-22.5] "around Toliara city in Madagascar"]
   ["K" [-23.355] "around Toliara city center in Madagascar"]
   ["S" [37] "around Central Japan"]
   ["S" [35.5] "around Tokyo city in Japan"]
   ["S" [35.69] "around Tokyo city center in Japan"]
   ; around the equator
   ["N" [1] "to the immediate north"]
   ["M" [-1] "to the immediate south"]
   ["N" [0] "with midpoint directly on it"]
   ["X" [83] "imediately south of north polar maximum"]
   ["Z" [85] "imediately north of north polar maximum"]
   ["X" [84] "directly on north polar maximum"]
   ["C" [-79] "imediately north of south polar minimum"]
   ["Z" [-81] "imediately south of south polar minimum"]
   ["C" [-80] "directly on south polar minimum"]])

(deftest utm-letter-designator
  (doseq [[result args] utm-letter-designator-test-cases]
    (is (= (apply usng/UTMLetterDesignator args) result))))

(def parse-usng-test-cases
  [[{:zone 5 :let "Q"} "5Q" "with single digit zone"]
   [{:zone 12 :let "S"} "12S" "with two digit zone"]
   [{:zone 5 :let "Q" :sq1 "K" :sq2 "B"} "5Q KB" "with single digit zone and squares"]
   [{:zone 12
     :let  "S"
     :sq1  "V"
     :sq2  "C"}
    "12S VC" "with two digit zone and squares"]
   [{:zone      5
     :let       "Q"
     :sq1       "K"
     :sq2       "B"
     :precision 5
     :east      "42785"
     :north     "31517"}
    "5Q KB 42785 31517"
    "with single digit zoneg squares and 5 digit meters"]
   [{:zone      12
     :let       "S"
     :sq1       "V"
     :sq2       "C"
     :precision 5
     :east      "12900"
     :north     "43292"}
    "12S VC 12900 43292"
    "with two digit zoneg squares and 5 digit meters"]])

(def usng-selectors
  {:zone      usng/usng-get-zone
   :let       (comp str usng/usng-get-letter)
   :sq1       (comp str usng/usng-get-column-letter)
   :sq2       (comp str usng/usng-get-row-letter)
   :precision usng/usng-get-precision
   :east      (comp str usng/usng-get-easting)
   :north     (comp str usng/usng-get-northing)})

(deftest parse-usng
  (doseq [[result s] parse-usng-test-cases]
    (let [usng (usng/str->usng s)]
      (doseq [[k v] result]
        (let [selector (k usng-selectors)]
          (is (= (selector usng) v)))))))

(def usng->utm-test-cases
  [[{:N 2131517 :E 242785 :zone 5 :letter "Q"}
    "5Q KB 42785 31517"
    "with single digit zone"]
   [{:N 3743292 :E 412900 :zone 12 :letter "S"}
    "12S VC 12900 43292"
    "with two digit zone"]])

(def utm-selectors
  {:N  (comp usng/parseInt usng/utm-get-northing)
   :E (comp usng/parseInt usng/utm-get-easting)
   :zone usng/utm-get-zone
   :letter (comp str usng/utm-get-lattitude-band)})

(deftest usng->utm
  (doseq [[result s] usng->utm-test-cases]
    (let [usng (usng/str->usng s)
          utm (usng/usng->utm usng)]
      (doseq [[k v] result]
        (let [selector (k utm-selectors)]
          (is (= (selector utm) v)))))))

(def NORTHING_OFFSET 10000000.0)

(def utm->dd-test-cases
  [[{:north 1 :east -156 :south 0 :west -157}
    {:northing 42785 :easting 131517 :zone 5 :accuracy 100000}
    "with single digit zone and specifying accuracy"]
   [{:lat 0 :lon -157}
    {:northing 42785 :easting 131517 :zone 5}
    "with single digit zone and not specifying accuracy"]
   [{:north 1 :east -115 :south 0 :west -116}
    {:northing 12900 :easting 43292 :zone 12 :accuracy 100000}
    "with two digit zone and specifying accuracy"]
   [{:lat 0 :lon -116}
    {:northing 12900 :easting 43292 :zone 12}
    "with two digit zone and not specifying accuracy"]
   [{:lat -35 :lon -59}
    {:northing (- 6168016 NORTHING_OFFSET) :easting 341475 :zone 21}
    "around Merlo town in Buenos Aires"]
   #_[{:lat -35 :lon -59}
      {:northing 6168016 :easting 341475 :zone 21 :ns "S"}
      "around Merlo town in Buenos Aires with N/S"]])

(defn floor [n]
  (Math/floor n))

(def lat-long-selectors
  {:lat (comp usng/parseInt floor usng/dd-get-lat)
   :lon (comp usng/parseInt floor usng/dd-get-lon)
   :north (comp usng/parseInt floor usng/bbox-get-north)
   :east (comp usng/parseInt floor usng/bbox-get-east)
   :south (comp usng/parseInt floor usng/bbox-get-south)
   :west (comp usng/parseInt floor usng/bbox-get-west)})

(defn run-test-case [tc]
  (let [[result args] tc
        accuracy  (:accuracy args)
        args ((juxt :easting :northing :zone) args)
        utm (apply usng/create-utm args)
        dd (if (some? accuracy)
             (usng/utm->bbox true utm accuracy)
             (usng/utm->dd true utm))]
    (doseq [[k v] result]
      (let [selector (k lat-long-selectors)]
        (is (= (selector dd) v))))))

(deftest utm->lat-lon
  (dorun (map run-test-case utm->dd-test-cases)))

(def usng->dd-test-cases
  [[{:north 19
     :east -156
     :south 19
     :west -156}
    "5Q KB 42785 31517"
    "with single digit zone"]
   [{:north 33
     :east -112
     :south 33
     :west -112}
    "12S VC 12900 43292"
    "with two digit zone"]])

(deftest usng->dd
  (doseq [[result s] usng->dd-test-cases]
    (let [usng (usng/str->usng s)
          dd (usng/usng->dd true usng false)]
      #_(doseq [[k v] result]
          (let [selector (k usng->dd-test-cases)]
            (is (= (selector dd) v)))))))

