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

(defn create-utm [utm]
  (reify UtmCoordinate
    (getEasting [this] (:E utm))
    (getNorthing [this] (:N utm))
    (getZoneNumber [this] (:zone utm))
    (getLattitudeBand [this] (:letter utm))
    (getPrecision [this])))

(defn from-utm [^UtmCoordinate utm]
  {:E (.getEasting utm)
   :N (.getNorthing utm)
   :zone (.getZoneNumber utm)
   :letter (.getLattitudeBand utm)})

(defn create-usng [usng]
  (reify UsngCoordinate
    (getZoneNumber [this] (:zone usng))
    (getLatitudeBandLetter [this]
      (if (string? (:let usng))
        (.charAt (:let usng) 0)
        (:let usng)))
    (getColumnLetter [this] (:sq1 usng))
    (getRowLetter [this] (:sq2 usng))
    (getEasting [this]
      (let [easting (:east usng)
            precision (:precision usng)]
        (if (and (some? easting) (> precision 1))
          (usng/parseInt easting))))
    (getNorthing [this]
      (let [northing (:north usng)
            precision (:precision usng)]
        (if (and (some? northing) (> precision 1))
          (usng/parseInt northing))))
    (getPrecision [this]
      (case (:precision usng)
        0 CoordinatePrecision/SIX_BY_EIGHT_DEGREES
        1 CoordinatePrecision/ONE_HUNDRED_KILOMETERS
        2 CoordinatePrecision/TEN_KILOMETERS
        3 CoordinatePrecision/ONE_KILOMETER
        4 CoordinatePrecision/ONE_HUNDRED_METERS
        5 CoordinatePrecision/TEN_METERS
        6 CoordinatePrecision/ONE_METER
        CoordinatePrecision/SIX_BY_EIGHT_DEGREES))
    (toMgrsString [this])
    Object
    (equals [this b] true)
    (toString [this] (usng/usng->str usng))))

(defn from-usng [^UsngCoordinate usng]
  {:zone      (.getZoneNumber usng)
   :let       (.getLatitudeBandLetter usng)
   :sq1       (.getColumnLetter usng)
   :sq2       (.getRowLetter usng)
   :precision (.getIntValue (.getPrecision usng))
   :east      (.getEasting usng)
   :north     (.getNorthing usng)})

(defn create-dd [dd]
  (reify DecimalDegreesCoordinate
    (getLat [this] (:lat dd))
    (getLon [this] (:lon dd))))

(defn from-dd [^DecimalDegreesCoordinate dd]
  {:lat (.getLat dd)
   :lon (.getLon dd)})

(defn create-bbox [bbox]
  (reify BoundingBox
    (getNorth [this] (:north bbox))
    (getSouth [this] (:south bbox))
    (getEast [this] (:east bbox))
    (getWest [this] (:west bbox))))

(defn from-bbox [^BoundingBox bbox]
  {:north (.getNorth bbox)
   :south (.getSouth bbox)
   :east (.getEast bbox)
   :west (.getWest bbox)})

(defn -init [isNad83Datum] [[] isNad83Datum])

(defn dispatch [this o & args] (class o))

(defmulti -toUtm dispatch)

(defmethod -toUtm UsngCoordinate ^UtmCoordinate [this usngCoordinate]
  (create-utm (usng/usng->utm (from-usng usngCoordinate))))

(defmethod -toUtm DecimalDegreesCoordinate ^UtmCoordinate [this decimalDegreesCoordinate]
  (let [isNad83? (.-state this)]
    (create-utm (usng/dd->utm isNad83? (from-dd decimalDegreesCoordinate)))))

(defmulti -toUsng dispatch)

(defmethod -toUsng BoundingBox ^UsngCoordinate [this latLonCoordinate]
  (let [isNad83? (.-state this)]
    (create-usng (usng/bbox->usng isNad83? (from-bbox latLonCoordinate)))))

(defmethod -toUsng DecimalDegreesCoordinate ^UsngCoordinate
  ([this decimalDegreesCoordinate]
   (let [isNad83? (.-state this)]
     (create-usng (usng/dd->usng isNad83? (from-dd decimalDegreesCoordinate) (inc (.getIntValue CoordinatePrecision/ONE_METER))))))
  ([this decimalDegreesCoordinate ^CoordinatePrecision coordinatePrecision]
   (let [isNad83? (.-state this)]
     (create-usng (usng/dd->usng isNad83? (from-dd decimalDegreesCoordinate) (inc (.getIntValue coordinatePrecision)))))))

(defn -toBoundingBox
  (^BoundingBox [this ^UsngCoordinate usngCoordinate]
   (let [isNad83? (.-state this)
         utm (usng/usng->utm (from-usng usngCoordinate))]
     (create-bbox (usng/utm->bbox isNad83? utm 0))))
  (^BoundingBox [this ^UtmCoordinate utmCoordinate accuracy]
   (let [isNad83? (.-state this)]
     (create-bbox (usng/utm->bbox isNad83? (from-utm utmCoordinate) accuracy)))))

(defn -parseUtmString ^UtmCoordinate [this ^String utmString])

(defn -parseUsngString ^UsngCoordinate [this ^String usngString])

(defn -parseMgrsString ^UsngCoordinate [this ^String mgrsString])

(def -getZoneNumber #(usng/getZoneNumber %2 %3))

(def -getUtmLetterDesignator #(usng/UTMLetterDesignator %2))

(defmulti -toLatLon dispatch)

(defmethod -toLatLon UtmCoordinate ^DecimalDegreesCoordinate [this utmCoordinate]
  (let [isNad83? (.-state this)]
    (create-dd (usng/utm->dd isNad83? (from-utm utmCoordinate)))))

(defmethod -toLatLon UsngCoordinate ^DecimalDegreesCoordinate [this usngCoordinate]
  (let [isNad83? (.-state this)]
    (create-dd (usng/usng->dd isNad83? (from-usng usngCoordinate) true))))
