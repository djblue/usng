(ns usng.js
  (:require [clojure.string :as str]
            [usng.core :as usng]))

(defn USNGtoLL [isNad83? usngStr_input getCenter]
  (usng/usng->dd isNad83? (usng/str->usng usngStr_input) getCenter))

(defn GUsngtoLL [isNad83? s]
  (if (fn? js/GLatLng)
    (let [latlng #js []]
      (USNGtoLL isNad83? s latlng)
      (js/GLatLng. (aget latlng 0) (aget latlng 1)))
    (throw (usng/error "GLatLng not defined."))))

(defn USNGtoUTM [zone letter sq1 sq2 east north ret]
  (let [precision (.-length north)
        usng (usng/create-usng zone letter sq1 sq2 precision east north)]
    (js/Object.assign ret (usng/usng->utm usng))
    ret))

(defn parseUSNG_str [input parts]
  (let [usng (usng/str->usng input)]
    (js/Object.assign parts usng)
    usng))

(defn LLtoUTM [isNad83? lat lon utmcoords zone]
  (let [dd (usng/create-dd lat lon)
        utm (usng/dd->utm isNad83? dd zone)]
    (aset utmcoords 0 (.-E utm))
    (aset utmcoords 1 (.-N utm))
    (aset utmcoords 2 (.-zone utm))))

(defn LLtoUTMwithNS [isNad83? lat lon utmcoords zone]
  (LLtoUTM isNad83? lat lon utmcoords zone)
  (if (< (aget utmcoords 1) 0)
    (do (aset utmcoords 1 (+ (aget utmcoords 1) usng/NORTHING_OFFSET))
        (aset utmcoords 3 "S"))
    (aset utmcoords 3 "N")))

(defn LLBboxtoUSNG [isNad83? north south east west]
  (let [bbox (usng/create-bbox north east south west)]
    (usng/usng->str (usng/bbox->usng isNad83? bbox))))

(defn LLtoUSNG [isNad83? lat lon precision]
  (let [dd (usng/create-dd lat lon)]
    (usng/usng->str (usng/dd->usng isNad83? dd precision))))


(defn LLtoMGRS [isNad83? lat lon precision]
  (usng/replace (LLtoUSNG isNad83? lat lon precision) #" " ""))

(defn LLtoUSNG_nad27 [lat lon precision]
  (let [usngstr (LLtoUSNG false lat lon precision)]
    (usng/str usngstr " (NAD27)")))

(defn UTMtoLL [isNad83? UTMNorthing UTMEasting UTMZoneNumber accuracy]
  (let [utm (usng/create-utm UTMEasting UTMNorthing UTMZoneNumber)]
    (if (some? accuracy)
      (usng/utm->bbox isNad83? utm accuracy)
      (usng/utm->dd isNad83? utm))))

(defn UTMtoLLwithNS [isNad83? UTMNorthing UTMEasting UTMZoneNumber accuracy NorSIndicator]
  (UTMtoLL isNad83? (- UTMNorthing (if (usng/= NorSIndicator "S") usng/NORTHING_OFFSET 0)) UTMEasting UTMZoneNumber accuracy))

(defn Converter [options]
  (let [isNad83? (not (and options
                           (.-datum options)
                           (identical? (str/upper-case (.-datum options)) "NAD27")))]
    #js {"LLBboxtoUSNG"        #(LLBboxtoUSNG isNad83? %1 %2 %3 %4)
         "LLtoUTM"             #(LLtoUTM isNad83? %1 %2 %3 %4)
         "LLtoUTMwithNS"       #(LLtoUTMwithNS isNad83? %1 %2 %3 %4)
         "UTMtoLLwithNS"       #(UTMtoLLwithNS isNad83? %1 %2 %3 %4 %5)
         "LLtoUSNG"            #(LLtoUSNG isNad83? %1 %2 %3)
         "UTMtoLL"             #(UTMtoLL isNad83? %1 %2 %3 %4)
         "USNGtoLL"            #(USNGtoLL isNad83? %1 %2)
         "LLtoMGRS"            #(LLtoMGRS isNad83? %1 %2 %3)
         "GUsngtoLL"           #(GUsngtoLL isNad83? %1)
         "getZoneNumber"       usng/getZoneNumber
         "LLtoKM"              usng/LLtoKM
         "UTMLetterDesignator" usng/UTMLetterDesignator
         "USNGtoUTM"           USNGtoUTM
         "parseUSNG_str"       parseUSNG_str
         "LLtoUSNG_nad27"      LLtoUSNG_nad27}))

(set! (.-Converter js/exports) Converter)

