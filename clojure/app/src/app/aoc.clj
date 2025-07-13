(ns aoc-2020-4
  (:require [clojure.string :as str]))

;; Sample input for testing
(def sample-input 
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(def input (str/split sample-input #"\n\n"))

(defn format-input [input]
  (map #(str/replace % #"\n" " ") input))

;; Part 1 - Fixed your broken substring matching
(defn validate-pp-part1 [passport]
  (let [required-fields ["byr:" "iyr:" "eyr:" "hgt:" "hcl:" "ecl:" "pid:"]]
    (every? #(str/includes? passport %) required-fields)))

;; Part 2 - Your regex was mostly fine, just cleaned up
(def validation-rules
  {:byr #"byr:(19[2-9][0-9]|200[0-2])(\s|$)"
   :iyr #"iyr:(201[0-9]|2020)(\s|$)"
   :eyr #"eyr:(202[0-9]|2030)(\s|$)"
   :hgt #"hgt:((59|6[0-9]|7[0-6])in|(1[5-8][0-9]|19[0-3])cm)(\s|$)"
   :hcl #"hcl:#[a-f0-9]{6}(\s|$)"
   :ecl #"ecl:(amb|blu|brn|gry|grn|hzl|oth)(\s|$)"
   :pid #"pid:[0-9]{9}(\s|$)"})

(defn validate-pp-part2 [passport]
  (every? #(re-find (% validation-rules) passport) 
          (keys validation-rules)))

(defn count-valid [passports validator]
  (->> passports
       (filter validator)
       count))

;; Results
(def formatted-input (format-input input))

;; Part 1
(println "Part 1:" (count-valid formatted-input validate-pp-part1))

;; Part 2  
(println "Part 2:" (count-valid formatted-input validate-pp-part2))
