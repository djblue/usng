// Copyright (c) 2009 Larry Moore, larmoor@gmail.com
//               2014 Mike Adair, Richard Greenwood, Didier Richard, Stephen Irons, Olivier Terral and Calvin Metcalf (proj4js)
//               2014 Codice Foundation
// Released under the MIT License; see
// http://www.opensource.org/licenses/mit-license.php
// or http://en.wikipedia.org/wiki/MIT_License
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

// This library was modified from the original usngs.js script to fix bugs, edge conditions, and limitations
// dealing with the different precision levels in usng/mgrs. It is by no means perfect! If you find a bug,
// submit it!

// ******************************* Constants ********************************

var PI = Math.PI
var DEG_2_RAD = PI / 180
var RAD_2_DEG = 180.0 / PI
var BLOCK_SIZE = 100000 // size of square identifier (within grid zone designation)

// For diagram of zone sets, please see the "United States National Grid" white paper.
var GRIDSQUARE_SET_COL_SIZE = 8 // column width of grid square set
var GRIDSQUARE_SET_ROW_SIZE = 20 // row height of grid square set

// UTM offsets
var EASTING_OFFSET = 500000.0 // (meters)
var NORTHING_OFFSET = 10000000.0 // (meters)

// scale factor of central meridian
var k0 = 0.9996
var USNGSqLetOdd = 'ABCDEFGHJKLMNPQRSTUV'
var USNGSqLetEven = 'FGHJKLMNPQRSTUVABCDE'

function repeat(str, n) {
  return str.repeat(n)
}

function map(f, coll) {
  return coll.map(f)
}

function str() {
  return Array.prototype.reduce
    .call(arguments, function (a, b) { return a + b }, '')
}

function char(n) { return String.fromCharCode(n) }
function charAt(str, n) { return str.charAt(n) }
function indexOf(s, value) { return s.indexOf(value) }
function subs(s, start, end) { return s.slice(start, end) }
function upperCase(s) { return s.toUpperCase() }

function even(n) { return n % 2 === 0 }

function replace(s, match, replacement) {
  return s.replace(match, replacement)
}

function zero(n) { return n === 0 }
function inc(n) { return n + 1 }
function dec(n) { return n - 1 }

function sin()    { return Math.sin.apply(null, arguments) }
function cos()    { return Math.cos.apply(null, arguments) }
function tan()    { return Math.tan.apply(null, arguments) }
function max()    { return Math.max.apply(null, arguments) }
function round()  { return Math.round.apply(null, arguments) }
function pow()    { return Math.pow.apply(null, arguments) }
function floor()  { return Math.floor.apply(null, arguments) }
function sqrt()   { return Math.sqrt.apply(null, arguments) }
function atan2()  { return Math.atan2.apply(null, arguments) }

function toFixed(n) { return n.toFixed() }

function mod(n, m) { return n % m }

function nth(coll, index) { return coll[index] }
function count(coll) { return coll.length }

function findIndex(coll, fn) {
  return coll.findIndex(fn)
}

function includes(s, substr) { return s.indexOf(substr) !== -1 }

function oget(o, k) { return o[k] }
function oset(o, k, v) { o[k] = v }
var aset = oset
var aget = nth

function isUndefined(v) {
  return v === undefined
}

function isFunction(v)  {
  return typeof v === 'function'
}

// Number of digits to display for x,y coords
//  One digit:    10 km precision      eg. "18S UJ 2 1"
//  Two digits:   1 km precision       eg. "18S UJ 23 06"
//  Three digits: 100 meters precision eg. "18S UJ 234 064"
//  Four digits:  10 meters precision  eg. "18S UJ 2348 0647"
//  Five digits:  1 meter precision    eg. "18S UJ 23480 06470"

// ********** retrieve zone number from latitude, longitude *************

// Zone number ranges from 1 - 60 over the range [-180 to +180]. Each
// range is 6 degrees wide. Special cases for points outside normal
// [-80 to +84] latitude zone.
function getZoneNumber(lat, lon) {
  var lat = parseFloat(lat),
      lon = parseFloat(lon)

  // sanity check on input
  if (lon > 360 || lon < -180 || lat > 84 || lat < -80) {
    throw new Error(str('usng.js, getZoneNumber: invalid input. lat: ', toFixed(lat, 4), ' lon: ', toFixed(lon, 4)))
  }

  // convert 0-360 to [-180 to 180] range
  var lonTemp = (lon + 180) - parseInt((lon + 180) / 360) * 360 - 180,
      zoneNumber = inc(parseInt((lonTemp + 180) / 6)),
      // Handle special case of west coast of Norway
      zoneNumber = (lat >= 56.0 && lat < 64.0 && lonTemp >= 3.0 && lonTemp < 12.0) ? 32 : zoneNumber

  // Special zones for Svalbard
  if (lat >= 72.0 && lat < 84.0) {
    if (lonTemp >= 0.0 && lonTemp < 9.0) {
      return 31
    } else if (lonTemp >= 9.0 && lonTemp < 21.0) {
      return 33
    } else if (lonTemp >= 21.0 && lonTemp < 33.0) {
      return 35
    } else if (lonTemp >= 33.0 && lonTemp < 42.0) {
      return 37
    } else {
      return zoneNumber
    }
  } else {
    return zoneNumber
  }
}

function LLtoKM(lat1, lon1, lat2, lon2) {
  var R = 6371000, // metres
      phi1 = lat1 * DEG_2_RAD,
      phi2 = lat2 * DEG_2_RAD,
      deltaPhi = (lat2 - lat1) * DEG_2_RAD,
      deltaLlamda = (lon2 - lon1) * DEG_2_RAD,
      a = sin(deltaPhi / 2) * sin(deltaPhi / 2) +
            cos(phi1) * cos(phi2) *
            sin(deltaLlamda / 2) * sin(deltaLlamda / 2),
      c = 2 * atan2(sqrt(a), sqrt(1 - a))

  return R * c
}

// this function does a very rough "best fit" to the center point
// this could definitely be improved
function LLBboxtoUSNG(LLtoUSNG) {
  return function (north, south, east, west) {
  var northNum = parseFloat(north),
      southNum = parseFloat(south),
      eastNum = parseFloat(east),
      westNum = parseFloat(west),
      // calculate midpoints for use in USNG string calculation
      lat = (northNum + southNum) / 2,
      lon = (eastNum + westNum) / 2,

      // round down edge cases
      lat = (lat >= 90) ? 89.9 : (lat <= -90) ? -89.9 : lat,
      lon = (lon >= 180) ? 179.9 : (lon <= -180) ? -179.9 : lon,

      // calculate distance between two points (North, West) and (South, East)
      R = 6371000, // metres
      phi1 = northNum * DEG_2_RAD,
      phi2 = southNum * DEG_2_RAD,
      deltaPhi = (southNum - northNum) * DEG_2_RAD,
      deltaLlamda = (westNum - eastNum) * DEG_2_RAD,
      // trigonometry calculate distance
      height = sin(deltaPhi / 2) * sin(deltaPhi / 2),
      height = R * 2 * atan2(sqrt(height), sqrt(1 - height)),
      length = cos(phi1) * cos(phi2) * sin(deltaLlamda / 2) * sin(deltaLlamda / 2),
      length = R * 2 * atan2(sqrt(length), sqrt(1 - length)),
      dist = max(height, length),

      // divide distance by square root of two
      lon = (zero(lon) && (eastNum > 90 || eastNum < -90) && (westNum > 90 || westNum < -90)) ? 180 : lon

  // calculate a USNG string with a precision based on distance
  // precision is defined in LLtoUSNG declaration
  // result is a USNG string of the form DDL LL DDDDD DDDDD
  // length of string will be based on the precision variable
  if (dist > 100000) {
    return LLtoUSNG(lat, lon, 0)
  } else if (dist > 10000) {
    return LLtoUSNG(lat, lon, 1)
  } else if (dist > 1000) {
    return LLtoUSNG(lat, lon, 2)
  } else if (dist > 100) {
    return LLtoUSNG(lat, lon, 3)
  } else if (dist > 10) {
    return LLtoUSNG(lat, lon, 4)
  } else if (dist > 1) {
    return LLtoUSNG(lat, lon, 5)
  } else if (dist >= 0) {
    return LLtoUSNG(lat, lon, 6)
  }
  }
}

// ************ retrieve grid zone designator letter **********************

// This routine determines the correct UTM letter designator for the given
// latitude returns 'Z' if latitude is outside the UTM limits of 84N to 80S

// Returns letter designator for a given latitude.
// Letters range from C (-80 lat) to X (+84 lat), with each zone spanning
// 8 degrees of latitude.
function UTMLetterDesignator(lat) {
  var lat = parseFloat(lat)

  if (lat > 84 || lat < -80) {
    return 'Z'

  } else {
    var index = (lat + 80) / 8,
        index = (index >= 6) ? inc(index) : index, // skip 'I'
        index = (index >= 12) ? inc(index) : index, // skip 'O'
        index = (index >= 22) ? dec(index) : index // adjust for 80 to 84, which should be 'X'

    return char(67 /* C */ + index)
  }
}

// *************** convert latitude, longitude to UTM  ******************

// Converts lat/long to UTM coords.  Equations from USGS Bulletin 1532
// (or USGS Professional Paper 1395 "Map Projections - A Working Manual",
// by John P. Snyder, U.S. Government Printing Office, 1987.)

// East Longitudes are positive, West longitudes are negative.
// North latitudes are positive, South latitudes are negative
// lat and lon are in decimal degrees

// output is in the input array utmcoords
// utmcoords[0] = easting
// utmcoords[1] = northing (NEGATIVE value in southern hemisphere)
// utmcoords[2] = zone
function LLtoUTM(EQUATORIAL_RADIUS, ECC_SQUARED) {
  return function (lat, lon, utmcoords, zone) {
    var squared = ECC_SQUARED,
        radius = EQUATORIAL_RADIUS,
        primeSquared = squared / (1 - squared),
        // utmcoords is a 2-D array declared by the calling routine
        // note: input of lon = 180 or -180 with zone 60 not allowed; use 179.9999
        lat = parseFloat(lat),
        lon = parseFloat(lon)

    // Constrain reporting USNG coords to the latitude range [80S .. 84N]
    if (lat > 84.0 || lat < -80.0) {
      return undefined
    } else {

      // sanity check on input - turned off when testing with Generic Viewer
      if (lon > 360 || lon < -180 || lat > 90 || lat < -90) {
        throw new Error(str('usng.js, LLtoUTM, invalid input. lat: ', toFixed(lat, 4), ' lon: ', toFixed(lon, 4)))
      }

      // Make sure the longitude is between -180.00 .. 179.99..
      // Convert values on 0-360 range to this range.
      var lonTemp = (lon + 180) - parseInt((lon + 180) / 360) * 360 - 180,
          latRad = lat * DEG_2_RAD,
          lonRad = lonTemp * DEG_2_RAD,

          // user-supplied zone number will force coordinates to be computed in a particular zone
          zoneNumber = (!zone) ? getZoneNumber(lat, lon) : zone,

          lonOrigin = dec(zoneNumber) * 6 - 180 + 3, // +3 puts origin in middle of zone
          lonOriginRad = lonOrigin * DEG_2_RAD,

          // compute the UTM Zone from the latitude and longitude
          UTMZone = str(zoneNumber, UTMLetterDesignator(lat), ' '),

          N = radius / sqrt(1 - squared * sin(latRad) * sin(latRad)),
          T = tan(latRad) * tan(latRad),
          C = primeSquared * cos(latRad) * cos(latRad),
          A = cos(latRad) * (lonRad - lonOriginRad),

          // Note that the term Mo drops out of the "M" equation, because phi
          // (latitude crossing the central meridian, lambda0, at the origin of the
          //  x,y coordinates), is equal to zero for UTM.
          M = radius * ((1 - squared / 4
            - 3 * (squared * squared) / 64
            - 5 * (squared * squared * squared) / 256) * latRad
            - (3 * squared / 8 + 3 * squared * squared / 32
            + 45 * squared * squared * squared / 1024)
            * sin(2 * latRad) + (15 * squared * squared / 256
            + 45 * squared * squared * squared / 1024) * sin(4 * latRad)
            - (35 * squared * squared * squared / 3072) * sin(6 * latRad)),

          UTMEasting = (k0 * N * (A + (1 - T + C) * (A * A * A) / 6
            + (5 - 18 * T + T * T + 72 * C - 58 * primeSquared)
            * (A * A * A * A * A) / 120)
            + EASTING_OFFSET),

          UTMNorthing = (k0 * (M + N * tan(latRad) * ((A * A) / 2 + (5 - T + 9
            * C + 4 * C * C) * (A * A * A * A) / 24
            + (61 - 58 * T + T * T + 600 * C - 330 * primeSquared)
            * (A * A * A * A * A * A) / 720)))

      aset(utmcoords, 0, UTMEasting)
      aset(utmcoords, 1, UTMNorthing)
      aset(utmcoords, 2, zoneNumber)
    }
  }
}

// *************** convert latitude, longitude to UTM  *******************
// Uses N or S indicator instead of returning a negative Northing value
function LLtoUTMwithNS(LLtoUTM) {
  return function(lat, lon, utmcoords, zone) {
    LLtoUTM(lat, lon, utmcoords, zone)
    if (aget(utmcoords, 1) < 0) {
      aset(utmcoords, 1, aget(utmcoords, 1) + NORTHING_OFFSET)
      aset(utmcoords, 3, 'S')
    } else {
      aset(utmcoords, 3, 'N')
    }
  }
}

// *************** convert latitude, longitude to UTM  *******************
// Uses N or S indicator instead of returning a negative Northing value
function UTMtoLLwithNS(UTMtoLL) {
  return function (UTMNorthing, UTMEasting, UTMZoneNumber, accuracy, NorSIndicator) {
    return UTMtoLL(UTMNorthing - (NorSIndicator === 'S' ? NORTHING_OFFSET : 0), UTMEasting, UTMZoneNumber, accuracy)
  }
}

// *************** convert latitude, longitude to USNG  *******************
// Converts lat/lng to USNG coordinates.  Calls LLtoUTM first, then
// converts UTM coordinates to a USNG string.

// Returns string of the format: DDL LL DDDD DDDD (4-digit precision), eg:
// "18S UJ 2286 0705" locates Washington Monument in Washington, D.C.
// to a 10-meter precision.

// Precision refers to levels of USNG precision. Ie a precision of
// 0 returns a string in the form DDL
// 1 returns a string in the form DDL LL
// 2 returns a string in the form DDL LL D D
// etc
function LLtoUSNG(LLtoUTM) {
  return function (lat, lon, precision) {

  // make lon between -180 & 180
  var lon = (lon < -180) ? lon + 360 : (lon > 180) ? lon - 360 : lon,
      // parse lat & long parameters to floats
      lat = parseFloat(lat),
      lon = parseFloat(lon),
      // convert lat/lon to UTM coordinates
      coords = []

  LLtoUTM(lat, lon, coords)

  var UTMEasting = aget(coords, 0),
      UTMNorthing = aget(coords, 1),

      // ...then convert UTM to USNG

      // southern hemisphere case
      // Use offset for southern hemisphere
      UTMNorthing = (lat < 0) ? UTMNorthing + NORTHING_OFFSET : UTMNorthing,

      zoneNumber = getZoneNumber(lat, lon),
      USNGLetters = findGridLetters(zoneNumber, UTMNorthing, UTMEasting),

      // UTM northing and easting is the analogue of USNG letters + USNG northing and easting
      // so remove the component of UTM northing and easting that corresponds with the USNG letters
      USNGNorthing = mod(round(UTMNorthing), BLOCK_SIZE),
      USNGEasting = mod(round(UTMEasting), BLOCK_SIZE),

      // parse precision to something we understand
      precision = (isUndefined(precision) || precision < 0) ? 0 : precision,

      // digitPrecision is to account for just the numerical portion of the USNG string
      // the last 0-10 characters of the USNG string
      digitPrecision = 0,

      // ensure that digitPrecision is between 0-5 because USNG is specified to up to 5 digits
      digitPrecision = (precision > 0) ? dec(precision) : digitPrecision,
      digitPrecision = (digitPrecision > 5) ? 5 : digitPrecision,

      // truncate USNG string digits to achieve specified precision
      USNGNorthing = floor(USNGNorthing / pow(10, (5 - digitPrecision))),
      USNGEasting = floor(USNGEasting / pow(10, (5 - digitPrecision))),

      // begin building USNG string "DDL"
      USNG = zoneNumber + UTMLetterDesignator(lat),

      // add 100k meter grid letters to USNG string "DDL LL"
      USNG = (precision >= 1) ? str(USNG, ' ', USNGLetters) : USNG

  // REVISIT: Modify to incorporate dynamic precision ?

  // if requested precision is higher than USNG northing or easting, pad front
  // with zeros

  // add easting and northing to USNG string "DDL LL D+ D+"
  if (digitPrecision >= 1) {
    return str(
      USNG,
      ' ',
      repeat('0', digitPrecision - count(str(USNGEasting))),
      USNGEasting,
      ' ',
      repeat('0', digitPrecision - count(str(USNGNorthing))),
      USNGNorthing
    )
  } else {
    // return USNG string of the form "DDL LL DDDDD DDDDD"
    // length of string depends on precision specified
    return USNG
  }
  }
}

// **************** Find the set for a given zone. ************************

// There are six unique sets, corresponding to individual grid numbers in
// sets 1-6, 7-12, 13-18, etc. Set 1 is the same as sets 7, 13, ..; Set 2
// is the same as sets 8, 14, ..

// See p. 10 of the "United States National Grid" white paper.
function findSet(zoneNum) {
  switch (mod(parseInt(zoneNum), 6)) {
    case 0: return 6
    case 1: return 1
    case 2: return 2
    case 3: return 3
    case 4: return 4
    case 5: return 5
    default: return -1
  }
}

// ************************************************************************
// Retrieve the Square Identification (two-character letter code), for the
// given row, column and set identifier (set refers to the zone set:
// zones 1-6 have a unique set of square identifiers; these identifiers are
// repeated for zones 7-12, etc.)

// See p. 10 of the "United States National Grid" white paper for a diagram
// of the zone sets.
function lettersHelper(setter, row, col) {
  // handle case of last row
  var row = dec(zero(row) ? GRIDSQUARE_SET_ROW_SIZE : row),
      // handle case of last column
      col = dec(zero(col) ? GRIDSQUARE_SET_COL_SIZE : col)

  switch (setter) {
    case 1: return str(nth('ABCDEFGH', col), nth(USNGSqLetOdd, row))
    case 2: return str(nth('JKLMNPQR', col), nth(USNGSqLetEven, row))
    case 3: return str(nth('STUVWXYZ', col), nth(USNGSqLetOdd, row))
    case 4: return str(nth('ABCDEFGH', col), nth(USNGSqLetEven, row))
    case 5: return str(nth('JKLMNPQR', col), nth(USNGSqLetOdd, row))
    case 6: return str(nth('STUVWXYZ', col), nth(USNGSqLetEven, row))
  }
}

// single-meter precision
// Get the position for the square identifier that contains the point
function squareIdentifier(row, north_1m) {
  if (north_1m < BLOCK_SIZE) {
    return row
  } else {
    return squareIdentifier(inc(row), north_1m - BLOCK_SIZE)
  }
}

// Retrieve the square identification for a given coordinate pair & zone
// See "lettersHelper" function documentation for more details.
function findGridLetters(zoneNum, northing, easting) {
  // cycle repeats (wraps) after 20 rows
  var row = mod(squareIdentifier(1, round(parseFloat(northing))), GRIDSQUARE_SET_ROW_SIZE),
  // cycle repeats (wraps) after 8 columns
      col = mod(squareIdentifier(0, round(parseFloat(easting))), GRIDSQUARE_SET_COL_SIZE)

  return lettersHelper(findSet(parseInt(zoneNum)), row, col)
}

// ************  convert UTM coords to decimal degrees *********************

// Equations from USGS Bulletin 1532 (or USGS Professional Paper 1395)
// East Longitudes are positive, West longitudes are negative.
// North latitudes are positive, South latitudes are negative.

// Expected Input args:
// UTMNorthing   : northing-m (numeric), eg. 432001.8
// southern hemisphere NEGATIVE from equator ('real' value - 10,000,000)
// UTMEasting    : easting-m  (numeric), eg. 4000000.0
// UTMZoneNumber : 6-deg longitudinal zone (numeric), eg. 18

// lat-lon coordinates are turned in the object 'ret' : ret.lat and ret.lon
function zoneNumberLons(zone) {
  var east = -180.0 + (6 * zone), west = east - 6

  return { east: east, west: west }
}

function createLats(south, north) {
  return { south: south, north: north }
}

function zoneLetterLats(letter) {
  switch (letter) {
    case 'C': return createLats(-80.0, -72.0)
    case 'D': return createLats(-72.0, -64.0)
    case 'E': return createLats(-64.0, -56.0)
    case 'F': return createLats(-56.0, -48.0)
    case 'G': return createLats(-48.0, -40.0)
    case 'H': return createLats(-40.0, -32.0)
    case 'J': return createLats(-32.0, -24.0)
    case 'K': return createLats(-24.0, -16.0)
    case 'L': return createLats(-16.0, -8.0)
    case 'M': return createLats(-8.0,  -0.01)
    case 'N': return createLats(0.01,  8.0)
    case 'P': return createLats(8.0,   16.0)
    case 'Q': return createLats(16.0,  24.0)
    case 'R': return createLats(24.0,  32.0)
    case 'S': return createLats(32.0,  40.0)
    case 'T': return createLats(40.0,  48.0)
    case 'U': return createLats(48.0,  56.0)
    case 'V': return createLats(56.0,  64.0)
    case 'W': return createLats(64.0,  72.0)
    case 'X': return createLats(72.0,  84.0)
  }
}

function UTMtoLL(EQUATORIAL_RADIUS, ECC_SQUARED) {
  return function(UTMNorthing, UTMEasting, UTMZoneNumber, accuracy) {
    var radius = EQUATORIAL_RADIUS,
        squared = ECC_SQUARED,
        primeSquared = squared / (1 - squared),
        e1 = (1 - sqrt(1 - squared)) / (1 + sqrt(1 - squared)),

    // remove 500,000 meter offset for longitude
        xUTM = parseFloat(UTMEasting) - EASTING_OFFSET,
        yUTM = parseFloat(UTMNorthing),
        zoneNumber = parseInt(UTMZoneNumber),

    // origin longitude for the zone (+3 puts origin in zone center)
        lonOrigin = dec(zoneNumber) * 6 - 180 + 3,

    // M is the "true distance along the central meridian from the Equator to phi
    // (latitude)
        M = yUTM / k0,
        mu = M / (radius * (1 - squared / 4 - 3 * squared *
              squared / 64 - 5 * squared * squared * squared / 256)),

    // phi1 is the "footprint latitude" or the latitude at the central meridian which
    // has the same y coordinate as that of the point (phi (lat), lambda (lon) ).
        phi1Rad = mu + (3 * e1 / 2 - 27 * e1 * e1 * e1 / 32) * sin(2 * mu)
              + (21 * e1 * e1 / 16 - 55 * e1 * e1 * e1 * e1 / 32) * sin(4 * mu)
              + (151 * e1 * e1 * e1 / 96) * sin(6 * mu),
        phi1 = phi1Rad * RAD_2_DEG,

    // Terms used in the conversion equations
        N1 = radius / sqrt(1 - squared * sin(phi1Rad) * sin(phi1Rad)),
        T1 = tan(phi1Rad) * tan(phi1Rad),
        C1 = primeSquared * cos(phi1Rad) * cos(phi1Rad),
        R1 = radius * (1 - squared) / pow(1 - squared * sin(phi1Rad) * sin(phi1Rad), 1.5),
        D = xUTM / (N1 * k0),

    // Calculate latitude, in decimal degrees
        lat = phi1Rad - (N1 * tan(phi1Rad) / R1) * (D * D / 2 - (5 + 3 * T1 + 10
              * C1 - 4 * C1 * C1 - 9 * primeSquared) * D * D * D * D / 24 + (61 + 90 *
              T1 + 298 * C1 + 45 * T1 * T1 - 252 * primeSquared - 3 * C1 * C1) * D * D *
              D * D * D * D / 720),
        lat = lat * RAD_2_DEG,

        lat = zero(lat) ? 0.001 : lat,
        // Calculate longitude, in decimal degrees
        lon = (D - (1 + 2 * T1 + C1) * D * D * D / 6 + (5 - 2 * C1 + 28 * T1 - 3 *
              C1 * C1 + 8 * primeSquared + 24 * T1 * T1) * D * D * D * D * D / 120) /
              cos(phi1Rad),
        lon = lonOrigin + lon * RAD_2_DEG,
        result = {}

    if (accuracy) {
      if (accuracy <= 100000) {
        var fn = UTMtoLL(EQUATORIAL_RADIUS, ECC_SQUARED),
            northEast = fn(UTMNorthing + accuracy, UTMEasting + accuracy, UTMZoneNumber)
        oset(result, 'north', oget(northEast, 'lat'))
        oset(result, 'east', oget(northEast, 'lon'))
        oset(result, 'south', lat)
        oset(result, 'west', lon)
      } else {
        var zoneLetter = UTMLetterDesignator(lat),
            lats = zoneLetterLats(zoneLetter),
            lons = zoneNumberLons(UTMZoneNumber)
        if (lats && lons) {
          oset(result, 'north', oget(lats, 'north'))
          oset(result, 'south', oget(lats, 'south'))
          oset(result, 'east', oget(lons, 'east'))
          oset(result, 'west', oget(lons, 'west'))
        }
      }
    } else {
      oset(result, 'lat', lat)
      oset(result, 'lon', lon)
    }
    return result
  }
}

// zoneBase - southern edge of N-S zones of millions of meters
// multiply zone bases by 1 million to get the proper length for each
var zoneBase = map(
  function (n) { return n * 1000000 },
  [1.1, 2.0, 2.8, 3.7, 4.6, 5.5, 6.4, 7.3, 8.2, 9.1, 0, 0.8, 1.7, 2.6, 3.5, 4.4, 5.3, 6.2, 7.0, 7.9]
)

// we can exploit the repeating behavior of northing to find what the total northing should be
// iterate through the horizontal zone bands until our northing is greater than the zoneBase of our zone
function totalNorthing(northing, letter) {
  return northing < aget(zoneBase, indexOf('CDEFGHJKLMNPQRSTUVWX', letter))
    ? totalNorthing(northing + 2000000, letter)
    : northing
}

function getNorthing(zone, letter, sq2, north) {
  if (sq2) {
    // northing goes from 0 - 1,900,000. A corresponds with 0, B corresponds with 200,000, V corresponds with 1,900,000
    // even numbered zones have the northing letters offset from the odd northing. So, F corresponds with 0, G corresponds
    // with 100,000 and E corresponds with 1,900,000
    var northingArray = even(zone) ? 'FGHJKLMNPQRSTUVABCDE' : 'ABCDEFGHJKLMNPQRSTUV',
    // if zone number is even, use northingArrayEven, if odd, use northingArrayOdd
    // similar to finding easting, the index of sq2 corresponds with the base easting
        northing = totalNorthing(indexOf(northingArray, sq2) * 100000, letter)

    return northing + (north ? parseFloat(north) * pow(10, 5 - count(north)) : 0)
  } else {
    // add approximately half of the height of one large region to ensure we're in the right zone
    return aget(zoneBase, indexOf('CDEFGHJKLMNPQRSTUVWX', letter)) + 499600
  }
}

// easting goes from 100,000 - 800,000 and repeats across zones
// A,J,S correspond with 100,000, B,K,T correspond with 200,000 etc
var eastingArray = ['', 'AJS', 'BKT', 'CLU', 'DMV', 'ENW', 'FPX', 'GQY', 'HRZ']

function getEasting(sq1, east) {
  // loop through eastingArray until sq1 is found
  // the index of the string the letter is in will be the base easting, as explained in the declaration
  // of eastingArray
  var i = findIndex(eastingArray, function (v) { return includes(v, sq1) }),

  // multiply by 100,000 to get the proper base easting
      easting = i * 100000

  // add the east parameter to get the total easting
  return east ? easting + parseFloat(east) * pow(10, 5 - count(east)) : easting
}

// ******************** USNG to UTM **************************************
// The Follwing functions are used to convert USNG Cords to UTM Cords.
// ***********************************************************************
// USNGtoUTM(zone,let,sq1,sq2,east,north,ret)
// Expected Input args:
// zone: Zone (integer), eg. 18
// let: Zone letter, eg S
// sq1:  1st USNG square letter, eg U
// sq2:  2nd USNG square Letter, eg J
// east:  Easting digit string, eg 4000
// north:  Northing digit string eg 4000
// ret:  saves zone,let,Easting and Northing as properties ret
function USNGtoUTM(zone, letter, sq1, sq2, east, north, ret) {
  oset(ret, 'N', parseInt(getNorthing(zone, letter, sq2, north)))
  oset(ret, 'E', parseInt(getEasting(sq1, east)))
  oset(ret, 'zone', zone)
  oset(ret, 'letter', letter)
}

// convert lower-case characters to upper case, remove space delimeters, separate string into parts
function parseUSNG_str(input, parts) {
  if (!input) {
    return 0
  } else {
    // put usgn string in 'standard' form with no space delimiters
    var usngStr = replace(replace(upperCase(input), /%20/g, ''), / /g, '')

    if (count(usngStr) < 2) {
      return 0
    } else {
      // break usng string into its component pieces
      // if 2 digit zone
      var n = parseFloat(nth(usngStr, 1)),
          is2DigitZone = !isNaN(n) && isFinite(n),
          zone = parseInt(subs(usngStr, 0, is2DigitZone ? 2 : 1)),
          j = is2DigitZone ? 2 : 1,
          _let = nth(usngStr, j),
          sq1 = nth(usngStr, j + 1),
          sq2 = nth(usngStr, j + 2),
          j = j + 3,
          precision = (count(usngStr) - j) / 2,
          east = subs(usngStr, j, j + precision),
          j = j + precision,
          j = (nth(usngStr, j) === ' ') ? inc(j) : j,
          north = subs(usngStr, j, j + precision)

      oset(parts, 'zone', zone)
      oset(parts, 'let', _let)
      oset(parts, 'sq1', sq1)
      oset(parts, 'sq2', sq2)
      oset(parts, 'precision', precision)
      oset(parts, 'east',  east)
      oset(parts, 'north', north)
    }
  }
}

// parse a USNG string and feed results to USNGtoUTM, then the results of that to UTMtoLL
function USNGtoLL(UTMtoLL) {
  return function (usngStr_input, getCenter) {

    var usngp = {}, coords = {}

    parseUSNG_str(usngStr_input, usngp)

    // convert USNG coords to UTM; this routine counts digits and sets precision
    USNGtoUTM(
      oget(usngp, 'zone'),
      oget(usngp, 'let'),
      oget(usngp, 'sq1'),
      oget(usngp, 'sq2'),
      oget(usngp, 'east'),
      oget(usngp, 'north'),
      coords)

    // southern hemisphere case
    if (oget(usngp, 'let') < 'N') {
      oset(coords, 'N', oget(coords, 'N') - NORTHING_OFFSET)
    }

    var accuracy = (!getCenter) ? 100000.0 / pow(10, oget(usngp, 'precision')) : undefined

    return UTMtoLL(oget(coords, 'N'), oget(coords, 'E'), oget(usngp, 'zone'), accuracy)
  }
}

// create a Military Grid Reference System string.  this is the same as a USNG string, but
//    with no spaces.  space delimiters are optional but allowed in USNG, but are not allowed
//    in MGRS notation.  but the numbers are the same.
function LLtoMGRS(LLtoUSNG) {
  return function (lat, lon, precision) {
    // remove space delimiters to conform to mgrs spec
    return replace(LLtoUSNG(lat, lon, precision), / /g, '')
  }
}

// wrapper function specific to Google Maps, to make a converstion to lat/lng return a GLatLon instance.
// takes a usng string, converts it to lat/lng using a call to USNGtoLL,
// and returns an instance of GLatLng
function GUsngtoLL(USNGtoLL) {
  return function(str) {
    if (isFunction(GLatLng)) {
      var latlng = []
      USNGtoLL(str, latlng)
      return new GLatLng(aget(latlng, 0), aget(latlng, 1))
    } else {
      throw new Error('GLatLng not defined.')
    }
  }
}

function LLtoUSNG_nad27(lat, lon, precision) {
  // set ellipsoid to Clarke 1866 (meters)
  var EQUATORIAL_RADIUS = 6378206.4,
      ECC_SQUARED = 0.006768658,
      fn = LLtoUSNG(LLtoUTM(EQUATORIAL_RADIUS, ECC_SQUARED)),
      usngstr = fn(lat, lon, precision)

  return str(usngstr, ' (NAD27)')
}

function Converter(options) {
  var IS_NAD83_DATUM = !(options && options.datum && upperCase(options.datum) === 'NAD27')  // if false, assumes NAD27 datum

  var EQUATORIAL_RADIUS = IS_NAD83_DATUM ? 6378137.0 : 6378206.4,
      ECC_SQUARED = IS_NAD83_DATUM ? 0.006694380023 : 0.006768658,
      a = LLtoUTM(EQUATORIAL_RADIUS, ECC_SQUARED),
      b = LLtoUTMwithNS(a),
      c = LLtoUSNG(a),
      d = LLBboxtoUSNG(c),
      e = UTMtoLL(EQUATORIAL_RADIUS, ECC_SQUARED),
      f = UTMtoLLwithNS(e),
      g = USNGtoLL(e),
      h = LLtoMGRS(c),
      i = GUsngtoLL(g)


  return {
    getZoneNumber,
    LLtoKM,
    LLBboxtoUSNG: d,
    LLtoUTM: a,
    LLtoUTMwithNS: b,
    UTMtoLLwithNS: f,
    LLtoUSNG: c,
    UTMLetterDesignator,
    UTMtoLL: e,
    USNGtoUTM,
    USNGtoLL: g,
    parseUSNG_str,
    LLtoMGRS: h,
    GUsngtoLL: i,
    LLtoUSNG_nad27
  }
}

exports.Converter = Converter
