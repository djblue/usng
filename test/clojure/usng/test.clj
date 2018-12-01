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
     :east      42785
     :north     31517}
    "5Q KB 42785 31517"
    "with single digit zoneg squares and 5 digit meters"]
   [{:zone      12
     :let       "S"
     :sq1       "V"
     :sq2       "C"
     :precision 5
     :east      12900
     :north     43292}
    "12S VC 12900 43292"
    "with two digit zoneg squares and 5 digit meters"]])

(def usng-selectors
  {:zone      usng/usng-get-zone
   :let       (comp str usng/usng-get-letter)
   :sq1       (comp str usng/usng-get-column-letter)
   :sq2       (comp str usng/usng-get-row-letter)
   :precision usng/usng-get-precision
   :east      usng/usng-get-easting
   :north     usng/usng-get-northing})

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
  [[{:north 19.2609 :east -155.4470 :south 19.2609 :west -155.4470}
    "5Q KB 42785 31517"
    "with single digit zone"]
   [{:north 33.8262 :east -111.9412 :south 33.8262 :west -111.9412}
    "12S VC 12900 43292"
    "with two digit zone"]
   [{:lat 38.8895 :lon -77.0352}
    "18S UJ 23487 06483"]
   [{:north 38.8895 :south 38.8895 :west -77.0352 :east -77.0351}
    "18S UJ 2349 0648"]
   [{:north 38.8896 :west -77.0361 :east -77.0350 :south 38.8887}
    "18S UJ 234 064"]
   [{:north 38.8942 :west -77.0406 :east -77.0294 :south 38.8850}
    "18S UJ 23 06"]
   [{:north 38.9224 :west -77.0736 :east -76.9610 :south 38.8304}
    "18S UJ 2 0"]
   [{:north 39.7440 :west -77.3039 :east -76.1671 :south 38.8260}
    "18S UJ"]
   [{:north 40 :west -84 :east -78 :south 32}
    "17S"]
   [{:north 32 :west -102 :east -96 :south 24}
    "14R"]])

(def lat-long-selectors-other
  {:lat usng/dd-get-lat
   :lon usng/dd-get-lon
   :north usng/bbox-get-north
   :east usng/bbox-get-east
   :south usng/bbox-get-south
   :west usng/bbox-get-west})

(defn essentially-equal [a b]
  (< (Math/abs (- (Math/abs a) (Math/abs b))) 0.001))

(deftest usng->dd
  (doseq [[result s] usng->dd-test-cases]
    (let [usng (usng/str->usng s)
          dd (usng/usng->dd true usng (contains? result :lat))]
      (doseq [[k v] result]
        (let [selector (k lat-long-selectors-other)]
          (is (essentially-equal (selector dd) v)))))))

(def bbox->usng-test-cases
  [["12S"
    {:north 37, :south 31, :east -108, :west -114}
    "around Arizona in the United States"]
   ["12S UD"
    {:north 34.55, :south 34.45, :east -112.4, :west -112.3}
    "around Prescott/Chino Valley in Arizona"]
   ["12S UD 7 1"
    {:north 34.5, :south 34.45, :east -112.4, :west -112.4}
    "around Prescott/Chino Valley in Arizona"]
   ["12S UD 65 24"
    {:north 34.55, :south 34.55, :east -112.465, :west -112.47}
    "immediately around Prescott city in Arizona"]
   ["12S UD 649 241"
    {:north 34.55, :south 34.55, :east -112.471, :west -112.472}
    "immediately around Prescott city in Arizona"]
   ["12S UD 6494 2412"
    {:north 34.55, :south 34.55, :east -112.472, :west -112.4719}
    "immediately around Prescott city in Arizona"]
   ["12S UD 64941 24126"
    {:north 34.55, :south 34.55, :east -112.472, :west -112.47199}
    "immediately around Prescott city in Arizona"]
   ["21H"
    {:north -30, :south -35, :east -53, :west -58}
    "around Uruguay"]
   ["21H UB"
    {:north -34.5, :south -35, :east -58.5, :west -58.5}
    "around Buenos Aires city in Argentina"]
   ["21H UB 41 63"
    {:north -34.665, :south -34.66, :east -58.73, :west -58.73}
    "around Merlo town in Buenos Aires"]
   ["38K"
    {:north -11, :south -26, :east 51, :west 42}
    "around Madagascar"]
   ["38K LA"
    {:north -21.9, :south -22, :east 43.7, :west 43.6}
    "around Toliara city in Madagascar"]
   ["38K LA 6 6"
    {:north -22, :south -22, :east 43.7, :west 43.65}
    "around Toliara city in Madagascar"]
   ["38K LV 66 12"
    {:north -23.395, :south -23.39, :east 43.7, :west 43.695}
    "around Toliara city center in Madagascar"]
   ["54S"
    {:north 41, :south 33, :east 143, :west 138}
    "around Central Japan"]
   ["54S UD"
    {:north 35, :south 35, :east 140, :west 139}
    "around Tokyo city in Japan"]
   ["54S UE 86 51"
    {:north 35.7, :south 35.7, :east 139.75, :west 139.745}
    "around Tokyo city center in Japan"]
   ["60R"
    {:north 34, :south 23, :east 179, :west 172}
    "to the immediate west"]
   ["1R"
    {:north 34, :south 23, :east -179, :west -172}
    "to the immediate east"]
   ["1R BM"
    {:north 28, :south 28, :east 179.9, :west -179.9}
    "with date line crossing the middle"]
   ["58N"
    {:north 8, :south 1, :east 166, :west 159}
    "to the immediate north"]
   ["58M"
    {:north -1, :south -8, :east 166, :west 159}
    "to the immediate south"]
   ["58N"
    {:north 8, :south -8, :east 166, :west 159}
    "with equator crossing the middle"]
   ["60N"
    {:north 8, :south 1, :east 179, :west 172}
    "to the immediate west and north"]
   ["60M"
    {:north -1, :south -8, :east 179, :west 172}
    "to the immediate west and south"]
   ["1N"
    {:north 8, :south 1, :east -179, :west -172}
    "to the immediate east and north"]
   ["1M"
    {:north -1, :south -8, :east -179, :west -172}
    "to the immediate east and south"]
   ["1N AA"
    {:north 0, :south 0, :east -179.9, :west 179.9}
    "with crossing of date line and equator at center point"]
   ["30R"
    {:north 34, :south 23, :east -1, :west -8}
    "to the immediate west"]
   ["31R"
    {:north 34, :south 23, :east 1, :west 8}
    "to the immediate east"]
   ["31R"
    {:north 34, :south 23, :east -1, :west 1}
    "with date line crossing the middle"]
   ["30N"
    {:north 8, :south 1, :east -1, :west -8}
    "to the immediate west and north"]
   ["30M"
    {:north -1, :south -8, :east -1, :west -8}
    "to the immediate west and south"]
   ["31N"
    {:north 8, :south 1, :east 8, :west 1}
    "to the immediate east and north"]
   ["31M"
    {:north -1, :south -8, :east 8, :west 1}
    "to the immediate east and south"]
   ["31N"
    {:north 8, :south -8, :east 1, :west -1}
    "with crossing of prime meridian and equator at center point"]])

(deftest bbox->usng
  (doseq [[result args] bbox->usng-test-cases]
    (let [bbox (apply usng/create-bbox
                      ((juxt :north :east :south :west) args))
          usng (usng/usng->str (usng/bbox->usng true bbox))]
      (is (= usng result)))))

(def dd->usng-test-cases
  [["12S WC 0 6"
    {:lat 34, :lon -111}
    "around Arizona in the United States"]
   ["12S UD 6 1"
    {:lat 34.5, :lon -112.5}
    "around Prescott/Chino Valley in Arizona"]
   ["12S UD 65 23"
    {:lat 34.545, :lon -112.465, :precision 3}
    "immediately around Prescott city in Arizona"]
   ["21H XE 4 0" {:lat -32.5, :lon -55.5} "around Uruguay"]
   ["21H UB 6 8"
    {:lat -34.5, :lon -58.5}
    "around Buenos Aires city in Argentina"]
   ["21H UB 41 63"
    {:lat -34.66, :lon -58.73, :precision 3}
    "around Merlo town in Buenos Aires"]
   ["38K PE 5 5" {:lat -18.5, :lon 46.5} "around Madagascar"]
   ["38K LA 4 1"
    {:lat -22.5, :lon 43.5}
    "around Toliara city in Madagascar"]
   ["38K LA 45 11"
    {:lat -22.5, :lon 43.5, :precision 3}
    "around Toliara city center in Madagascar"]
   ["54S VF 5 9" {:lat 37, :lon 140.5} "around Central Japan"]
   ["54S UE 6 2" {:lat 35.5, :lon 139.5} "around Tokyo city in Japan"]
   ["54S UE 88 50"
    {:lat 35.69, :lon 139.77, :precision 3}
    "around Tokyo city center in Japan"]
   ["60R US 5 5"
    {:lat 28.5, :lon 175.5}
    "to the immediate west of the international date line"]
   ["1R FM 4 5"
    {:lat 28.5, :lon -175.5}
    "to the immediate east of the international date line"]
   ["1R BM 0 5"
    {:lat 28.5, :lon 180}
    "with date line crossing the middle of the international date line"]
   ["58N BK 2 9"
    {:lat 4.5, :lon 162.5}
    "to the immediate north of the equator"]
   ["58M BA 2 0"
    {:lat -4.5, :lon 162.5}
    "to the immediate south of the equator"]
   ["58N BF 2 0" {:lat 0, :lon 162.5} "with equator crossing"]
   ["60N UK 3 9"
    {:lat 4.5, :lon 175.5}
    "to the immediate west and north of the international date line and equator"]
   ["60M UA 3 0"
    {:lat -4.5, :lon 175.5}
    "to the immediate west and south of the international date line and equator"]
   ["1N FE 6 9"
    {:lat 4.5, :lon -175.5}
    "to the immediate east and north of the international date line and equator"]
   ["1M FR 6 0"
    {:lat -4.5, :lon -175.5}
    "to the immediate east and south of the international date line and equator"]
   ["1N AA 6 0"
    {:lat 0, :lon 180}
    "with crossing of date line and equator at center point"]
   ["30R US 5 5"
    {:lat 28.5, :lon -4.5}
    "to the immediate west of the prime meridian"]
   ["31R FM 4 5"
    {:lat 28.5, :lon 4.5}
    "to the immediate east of the prime meridian"]
   ["31R BM 0 5"
    {:lat 28.5, :lon 0}
    "with date line crossing the middle of the prime meridian"]
   ["30N UK 3 9"
    {:lat 4.5, :lon -4.5}
    "to the immediate west and north of the prime meridian and equator"]
   ["30M UA 3 0"
    {:lat -4.5, :lon -4.5}
    "to the immediate west and south of the prime meridian and equator"]
   ["31N FE 6 9"
    {:lat 4.5, :lon 4.5}
    "to the immediate east and north of the prime meridian and equator"]
   ["31M FR 6 0"
    {:lat -4.5, :lon 4.5}
    "to the immediate east and south of the prime meridian and equator"]
   ["31N AA 6 0"
    {:lat 0, :lon 0}
    "with crossing of prime meridian and equator at center point"]])

(deftest dd->usng
  (doseq [[result args] dd->usng-test-cases]
    (let [dd (apply usng/create-dd ((juxt :lat :lon) args))
          precision (get args :precision 2)
          usng (usng/usng->str (usng/dd->usng true dd precision))]
      (is (= usng result)))))

(def dd->utm-test-cases
  [[{:E 500000, :N 3762155, :zone 12}
    {:lat 34, :lon -111}
    "around Arizona in the United States"]
   [{:E 362289, :N 3818618, :zone 12}
    {:lat 34.5, :lon -112.5}
    "around Prescott/Chino Valley in Arizona"]
   [{:E 365575, :N 3823561, :zone 12}
    {:lat 34.545, :lon -112.465}
    "immediately around Prescott city in Arizona"]
   [{:E 640915, :N -3596850, :zone 21}
    {:lat -32.5, :lon -55.5}
    "around Uruguay"]
   [{:E 362289, :N -3818618, :zone 21}
    {:lat -34.5, :lon -58.5}
    "around Buenos Aires city in Argentina"]
   [{:E 341475, :N -3836700, :zone 21}
    {:lat -34.66, :lon -58.73}
    "around Merlo town in Buenos Aires"]
   [{:E 658354, :N -2046162, :zone 38}
    {:lat -18.5, :lon 46.5}
    "around Madagascar"]
   [{:E 345704, :N -2488944, :zone 38}
    {:lat -22.5, :lon 43.5}
    "around Toliara city in Madagascar"]
   [{:E 364050, :N -2583444, :zone 38}
    {:lat -23.355, :lon 43.67}
    "around Toliara city center in Madagascar"]
   [{:E 455511, :N 4094989, :zone 54}
    {:lat 37, :lon 140.5}
    "around Central Japan"]
   [{:E 363955, :N 3929527, :zone 54}
    {:lat 35.5, :lon 139.5}
    "around Tokyo city in Japan"]
   [{:E 388708, :N 3950262, :zone 54}
    {:lat 35.69, :lon 139.77}
    "around Tokyo city center in Japan"]
   [{:E 353193, :N 3153509, :zone 60}
    {:lat 28.5, :lon 175.5}
    "to the immediate west of the international date line"]
   [{:E 646806, :N 3153509, :zone 1}
    {:lat 28.5, :lon -175.5}
    "to the immediate east of the international date line"]
   [{:E 206331, :N 3156262, :zone 1}
    {:lat 28.5, :lon 180}
    "with date line crossing the middle"]
   [{:E 222576, :N 497870, :zone 58}
    {:lat 4.5, :lon 162.5}
    "to the immediate north of the equator"]
   [{:E 222576, :N -497870, :zone 58}
    {:lat -4.5, :lon 162.5}
    "to the immediate south of the equator"]
   [{:E 221723, :N 0, :zone 58}
    {:lat 0, :lon 162.5}
    "with equator crossing the middle"]
   [{:E 333579, :N 497566, :zone 60}
    {:lat 4.5, :lon 175.5}
    "to the immediate west and north of the international date line and equator"]
   [{:E 333579, :N -497566, :zone 60}
    {:lat -4.5, :lon 175.5}
    "to the immediate west and south of the international date line and equator"]
   [{:E 666420, :N 497566, :zone 1}
    {:lat 4.5, :lon -175.5}
    "to the immediate east and north of the international date line and equator"]
   [{:E 666420, :N -497566, :zone 1}
    {:lat -4.5, :lon -175.5}
    "to the immediate east and south of the international date line and equator"]
   [{:E 166021, :N 0, :zone 1}
    {:lat 0, :lon 180}
    "with crossing of date line and equator at center point"]
   [{:E 353193, :N 3153509, :zone 30}
    {:lat 28.5, :lon -4.5}
    "to the immediate west of the prime meridian"]
   [{:E 646806, :N 3153509, :zone 31}
    {:lat 28.5, :lon 4.5}
    "to the immediate east of the prime meridian"]
   [{:E 206331, :N 3156262, :zone 31}
    {:lat 28.5, :lon 0}
    "with date line crossing the middle of the prime meridian"]
   [{:E 333579, :N 497566, :zone 30}
    {:lat 4.5, :lon -4.5}
    "to the immediate west and north of the prime meridian and equator"]
   [{:E 333579, :N -497566, :zone 30}
    {:lat -4.5, :lon -4.5}
    "to the immediate west and south of the prime meridian and equator"]
   [{:E 666420, :N 497566, :zone 31}
    {:lat 4.5, :lon 4.5}
    "to the immediate east and north of the prime meridian and equator"]
   [{:E 666420, :N -497566, :zone 31}
    {:lat -4.5, :lon 4.5}
    "to the immediate east and south of the prime meridian and equator"]
   [{:E 166021, :N 0, :zone 31}
    {:lat 0, :lon 0}
    "with crossing of prime meridian and equator at center point"]])

(deftest dd->utm
  (doseq [[result args] dd->utm-test-cases]
    (let [dd (apply usng/create-dd ((juxt :lat :lon) args))
          utm (->> (usng/dd->utm true dd)
                   (map #(let [[k v] %]
                           [k (usng/parseInt v)]))
                   (into {}))
          utm (select-keys utm (keys result))]
      (is (= utm result)))))

(def dd->utm-with-ns-teset-cases
  [[{:E 500000, :N 3762155, :zone 12, :letter "N"}
    {:lat 34, :lon -111}
    "around Arizona in the United States with N/S"]
   [{:E 362289, :N 3818618, :zone 12, :letter "N"}
    {:lat 34.5, :lon -112.5}
    "around Prescott/Chino Valley in Arizona with N/S"]
   [{:E 365575, :N 3823561, :zone 12, :letter "N"}
    {:lat 34.545, :lon -112.465}
    "immediately around Prescott city in Arizona with N/S"]
   [{:E 640915, :N 6403149, :zone 21, :letter "S"}
    {:lat -32.5, :lon -55.5}
    "around Uruguay with N/S"]
   [{:E 362289, :N 6181381, :zone 21, :letter "S"}
    {:lat -34.5, :lon -58.5}
    "around Buenos Aires city in Argentina with N/S"]
   [{:E 341475, :N 6163299, :zone 21, :letter "S"}
    {:lat -34.66, :lon -58.73}
    "around Merlo town in Buenos Aires with N/S"]
   [{:E 658354, :N 7953837, :zone 38, :letter "S"}
    {:lat -18.5, :lon 46.5}
    "around Madagascar with N/S"]
   [{:E 345704, :N 7511055, :zone 38, :letter "S"}
    {:lat -22.5, :lon 43.5}
    "around Toliara city in Madagascar with N/S"]
   [{:E 364050, :N 7416555, :zone 38, :letter "S"}
    {:lat -23.355, :lon 43.67}
    "around Toliara city center in Madagascar with N/S"]])

(deftest dd->utm
  (doseq [[result args] dd->utm-with-ns-teset-cases]
    (let [dd (apply usng/create-dd ((juxt :lat :lon) args))
          utm (->> (usng/dd->utm-with-ns true dd)
                   (map #(let [[k v] %]
                           [k (if-not (number? v)
                                v
                                (usng/parseInt v))]))
                   (into {}))
          utm (select-keys utm (keys result))]
      (is (= utm result)))))

(def github-data
  [{:description "washington monument"
    :lat 38.8895
    :lon -77.0352
    :N 4306483
    :E 323486
    :zone 18
    :usng "18S UJ 23487 06483"}
   {:description "white house"
    :lat 38.8977
    :lon -77.0366
    :N 4307395
    :E 323385
    :zone 18
    :usng "18S UJ 23386 07396"}
   {:description "mount everest"
    :lat 27.9881
    :lon 86.9253
    :N 3095886
    :E 492654
    :zone 45
    :usng "45R VL 92654 95886"}
   {:description "hollywood sign"
    :lat 34.1341
    :lon -118.3217
    :N 3777813
    :E 378131
    :zone 11
    :usng "11S LT 78132 77814"}
   {:description "empire state building"
    :lat 40.7484
    :lon -73.9857
    :N 4511322
    :E 585628
    :zone 18
    :usng "18T WL 85628 11322"}
   {:description "arlington cemetery"
    :lat 38.88
    :lon -77.07
    :N 4305496
    :E 320444
    :zone 18
    :usng "18S UJ 20444 05497"}
   {:description "raven's stadium"
    :lat 39.277881
    :lon -76.622639
    :N 4348868
    :E 360040
    :zone 18
    :usng "18S UJ 60040 48869"}
   {:description "independence hall"
    :lat 39.9489
    :lon -75.15
    :N 4422096
    :E 487186
    :zone 18
    :usng "18S VK 87187 22096"}
   {:description "naval air station oceana"
    :lat 36.8206
    :lon -76.0333
    :N 4075469
    :E 407844
    :zone 18
    :usng "18S VF 07844 75469"}
   {:description "uss north carolina"
    :lat 34.2364
    :lon -77.9542
    :N 3792316
    :E 227899
    :zone 18
    :usng "18S TC 27900 92317"}
   {:description "m-80-n and n-606 junction"
    :lat -36.0872
    :lon -72.8078
    :N (- 6004156 10000000.0)
    :E 697374
    :zone 18
    :usng "18H XF 97375 04155"}
   {:description "cobquecura"
    :lat -36.1333
    :lon -72.7833
    :N (- 5998991 10000000.0)
    :E 699464
    :zone 18
    :usng "18H XE 99464 98991"}
   {:description "aerodromo los morros (scqr)"
    :lat -36.1222
    :lon -72.8044
    :N (- 6000266 10000000.0)
    :E 697593
    :zone 18
    :usng "18H XF 97593 00265"}])

(deftest github-tests
  (doseq [data github-data]
    (is true)
    #_(println data)))

(def bbox->usng-test-cases
  [{:usng "18S UJ 23487 06483"
    :lat 38.8895
    :lon -77.0352
    :lon2 -77.0352}
   {:usng "18S UJ 2349 0648"
    :lat 38.8895
    :lon -77.0352
    :lon2 -77.0351}
   {:usng "18S UJ 234 064"
    :lat 38.8895
    :lon -77.0352
    :lon2 -77.035}
   {:usng "18S UJ 23 06"
    :lat 38.8895
    :lon -77.0352
    :lon2 -77.033}
   {:usng "18S UJ 2 0"
    :lat 38.8895
    :lon -77.0352
    :lon2 -77.06}
   {:usng "18S UJ"
    :lat 38.8895
    :lon -77.0352
    :lon2 -77.2}
   {:usng "17S"
    :lat 38.8895
    :lon -77.0352
    :lon2 -80}])

(deftest bbox->usng
  (doseq [tc bbox->usng-test-cases]
    (let [args ((juxt :lat :lon :lat :lon2) tc)
          bbox (apply usng/create-bbox args)
          usng (usng/bbox->usng true bbox)
          usng (usng/usng->str usng)]
      (= usng (:usng tc)))))
