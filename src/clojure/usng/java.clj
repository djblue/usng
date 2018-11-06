(ns usng.java
  (:require [usng.core :as usng])
  (:import (org.codice.usng4j UsngCoordinate
                              UtmCoordinate
                              BoundingBox
                              DecimalDegreesCoordinate
                              CoordinatePrecision)
           (org.codice.usng4j.impl UsngCoordinateImpl
                                   UtmCoordinateImpl
                                   BoundingBoxImpl
                                   DecimalDegreesCoordinateImpl))
  (:gen-class
    :prefix "-"
    :main false
    :constructors {[boolean] []}
    :init init
    :state state
    :methods [[getZoneNumber [double double] int]
              [getUtmLetterDesignator [double] String]
              [toBoundingBox [org.codice.usng4j.UtmCoordinate Integer] org.codice.usng4j.BoundingBox]]
    :implements [org.codice.usng4j.CoordinateSystemTranslator]
    :name org.codice.usng4j.impl.CoordinateSystemTranslatorImpl))

(defn -init [isNad83Datum] [[] isNad83Datum])

(defn dispatch [this o & args] (class o))

(defmulti -toUtm dispatch)

(defmethod -toUtm UsngCoordinate ^UtmCoordinate [this usngCoordinate]
  (usng/usng->utm usngCoordinate))

(defmethod -toUtm DecimalDegreesCoordinate ^UtmCoordinate [this decimalDegreesCoordinate]
  (let [isNad83? (.-state this)]
    (usng/dd->utm isNad83? decimalDegreesCoordinate)))

(defmulti -toUsng dispatch)

(defmethod -toUsng BoundingBox ^UsngCoordinate [this latLonCoordinate]
  (let [isNad83? (.-state this)]
    (usng/bbox->usng isNad83? latLonCoordinate)))

(defmethod -toUsng DecimalDegreesCoordinate ^UsngCoordinate
  ([this decimalDegreesCoordinate]
   (let [isNad83? (.-state this)]
     (usng/dd->usng isNad83? decimalDegreesCoordinate (inc (.getIntValue CoordinatePrecision/ONE_METER)))))
  ([this decimalDegreesCoordinate ^CoordinatePrecision coordinatePrecision]
   (let [isNad83? (.-state this)]
     (usng/dd->usng isNad83? decimalDegreesCoordinate (inc (.getIntValue coordinatePrecision))))))

(defn -toBoundingBox
  (^BoundingBox [this ^UsngCoordinate usngCoordinate]
   (let [isNad83? (.-state this)
         utm (usng/usng->utm usngCoordinate)]
     (usng/utm->bbox isNad83? utm 0)))
  (^BoundingBox [this ^UtmCoordinate utmCoordinate accuracy]
   (let [isNad83? (.-state this)]
     (usng/utm->bbox isNad83? utmCoordinate accuracy))))

(defn -parseUtmString ^UtmCoordinate [this ^String utmString])

(defn -parseUsngString ^UsngCoordinate [this ^String usngString])

(defn -parseMgrsString ^UsngCoordinate [this ^String mgrsString])

(def -getZoneNumber #(usng/getZoneNumber %2 %3))

(def -getUtmLetterDesignator #(usng/UTMLetterDesignator %2))

(defmulti -toLatLon dispatch)

(defmethod -toLatLon UtmCoordinate ^DecimalDegreesCoordinate [this utmCoordinate]
  (let [isNad83? (.-state this)]
    (usng/utm->dd isNad83? utmCoordinate)))

(defmethod -toLatLon UsngCoordinate ^DecimalDegreesCoordinate [this usngCoordinate]
  (let [isNad83? (.-state this)]
    (usng/usng->dd isNad83? usngCoordinate true)))
