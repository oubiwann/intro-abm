(ns pembas.tests.unit.model.personality.util.charge
  (:require
   [clojure.test :refer :all]
   [pembas.netlogo :as netlogo]))

(def model "PEMBAs-and-Crowds-Personality.nlogo")

(deftest coord-extraction
  (let [ws (netlogo/create-workspace model)]
    (is (= "3" (netlogo/report ws "x-coord" [3 4])))
    (is (= "4" (netlogo/report ws "y-coord" [3 4])))
    (is (= "4" (netlogo/report ws "dist-coord" [3 4 4])))
    (is (= "[]" (netlogo/report ws "dist-coord" [3 4])))
    (is (= "0.25" (netlogo/report ws "charge-coord" [3 4 4 0.25])))
    (is (= "[]" (netlogo/report ws "charge-coord" [3 4 4])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest summed-coords
  (let [ws (netlogo/create-workspace model)]
    (is (= "[0 0]"
        (netlogo/report ws "summed-coords" [[0 0]])))
    (is (= "[1 1]"
        (netlogo/report ws "summed-coords" [[1 1]])))
    (is (= "[-1 -1]"
        (netlogo/report ws "summed-coords" [[-1 -1]])))
    (is (= "[1 1]"
        (netlogo/report ws "summed-coords" [[0 0] [1 1]])))
    (is (= "[0 0]"
        (netlogo/report ws "summed-coords" [[-1 -1] [1 1]])))
    (is (= "[3 3]"
        (netlogo/report ws "summed-coords" [[0 0] [1 1] [2 2]])))
    (is (= "[9 12]"
        (netlogo/report ws "summed-coords" [[1 2] [3 4] [5 6]])))))

(deftest center-of-coords
  (let [ws (netlogo/create-workspace model)]
    (is (= "[0 0]"
        (netlogo/report ws "center-of-coords" [[0 0]])))
    (is (= "[1 1]"
        (netlogo/report ws "center-of-coords" [[1 1]])))
    (is (= "[-1 -1]"
        (netlogo/report ws "center-of-coords" [[-1 -1]])))
    (is (= "[0.5 0.5]"
        (netlogo/report ws "center-of-coords" [[0 0] [1 1]])))
    (is (= "[0 0]"
        (netlogo/report ws "center-of-coords" [[-1 -1] [1 1]])))
    (is (= "[1 1]"
        (netlogo/report ws "center-of-coords" [[0 0] [1 1] [2 2]])))
    (is (= "[3 4]"
        (netlogo/report ws "center-of-coords" [[1 2] [3 4] [5 6]])))
    (is (= "[-0.5 -0.5]"
        (netlogo/report ws "center-of-coords" [[1 0] [0 2] [-3 0] [0 -4]])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest charge
  (let [ws (netlogo/create-workspace model)]
    (is (= "0.1111111111111111"
           (netlogo/report ws "charge" 3)))
    (is (= "0.027777777777777776"
           (netlogo/report ws "charge" 6)))
    (is (= "0.020408163265306124"
           (netlogo/report ws "charge" 7)))))

(deftest dist-or-charge
  (let [ws (netlogo/create-workspace model)]
    (is (= "0.25"
           (netlogo/report ws "dist-or-charge" [0 1 2])))
    (is (= "0.25"
           (netlogo/report ws "dist-or-charge" [0 1 0 0.25])))))

(deftest x-coord-times-charge
  (let [ws (netlogo/create-workspace model)]
    (is (= "0.5"
           (netlogo/report ws "x-coord-times-charge" [2 3 2])))
    (is (= "0.5"
           (netlogo/report ws "x-coord-times-charge" [2 3 0 0.25])))))

(deftest x-coords-times-charge
  (let [ws (netlogo/create-workspace model)]
    (is (= "[0.25 0.75]"
           (netlogo/report ws "x-coords-times-charge" [[1 2 2] [3 4 2]])))
    (is (= "[0.25 0.75]"
           (netlogo/report ws "x-coords-times-charge" [[1 2 2] [3 4 0 0.25]])))))

(deftest y-coord-times-charge
  (let [ws (netlogo/create-workspace model)]
    (is (= "0.75"
           (netlogo/report ws "y-coord-times-charge" [2 3 2])))
    (is (= "0.75"
           (netlogo/report ws "y-coord-times-charge" [2 3 0 0.25])))))

(deftest y-coords-times-charge
  (let [ws (netlogo/create-workspace model)]
    (is (= "[0.5 1]"
           (netlogo/report ws "y-coords-times-charge" [[1 2 2] [3 4 2]])))
    (is (= "[0.5 1]"
           (netlogo/report ws "y-coords-times-charge" [[1 2 2] [3 4 0 0.25]])))))

(deftest summed-charge
  (let [ws (netlogo/create-workspace model)]
    (is (= "0.20011337868480725"
           (netlogo/report ws "summed-charge" [[1 1 7]
                                               [2 2 3]
                                               [3 3 7]
                                               [4 4 6]
                                               [4 4 7]])))
    (is (= "1.4236111111111112"
           (netlogo/report ws "summed-charge" [[1 0 1]
                                               [0 2 2]
                                               [-3 0 3]
                                               [0 -4 4]])))
    (is (= "10"
           (netlogo/report ws "summed-charge" [[1 0 0 1]
                                               [0 2 0 2]
                                               [-3 0 0 3]
                                               [0 -4 0 4]])))))

(deftest center-of-charge
  (let [ws (netlogo/create-workspace model)]
    (is (= "[0 0]"
           (netlogo/report ws "center-of-charge" [[1 0 1]
                                                  [0 1 1]
                                                  [-1 0 1]
                                                  [0 -1 1]])))
    (is (= "[1.1111111111111112 1.1111111111111112]"
           (netlogo/report ws "center-of-charge" [[1 1 1]
                                                  [2 2 (Math/sqrt 8)]])))
    (is (= "[2.3076923076923075 2.3076923076923075]"
           (netlogo/report ws "center-of-charge" [[2 2 (Math/sqrt 8)]
                                                  [3 3 (Math/sqrt 18)]])))
    (is (= "[3.36 3.36]"
           (netlogo/report ws "center-of-charge" [[3 3 (Math/sqrt 18)]
                                                  [4 4 (Math/sqrt 32)]])))
    (is (= "[1.2722063037249285 1.2722063037249285]"
           (netlogo/report ws "center-of-charge" [[1 1 1]
                                                  [2 2 (Math/sqrt 8)]
                                                  [3 3 (Math/sqrt 18)]
                                                  [4 4 (Math/sqrt 32)]])))
    (is (= "[0.4682926829268293 0.17560975609756097]"
           (netlogo/report ws "center-of-charge" [[1 0 1]
                                                  [0 2 2]
                                                  [-3 0 3]
                                                  [0 -4 4]])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest charge-k
  (let [ws (netlogo/create-workspace model)]
    (is (= "1.1111111111111112"
           (netlogo/report ws "charge-k" 10 3)))
    (is (= "0.2777777777777778"
           (netlogo/report ws "charge-k" 10 6)))
    (is (= "0.20408163265306123"
           (netlogo/report ws "charge-k" 10 7)))))

(deftest dist-or-charge-k
  (let [ws (netlogo/create-workspace model)]
    (is (= "2.5"
           (netlogo/report ws "dist-or-charge-k" 10 [0 1 2])))
    (is (= "0.25"
           (netlogo/report ws "dist-or-charge-k" 10 [0 1 0 0.25])))))

(deftest x-coord-times-charge-k
  (let [ws (netlogo/create-workspace model)]
    (is (= "10"
           (netlogo/report ws "x-coord-times-charge-k" 20 [2 3 2])))
    (is (= "0.5"
           (netlogo/report ws "x-coord-times-charge-k" 20 [2 3 0 0.25])))))

(deftest x-coords-times-charge-k
  (let [ws (netlogo/create-workspace model)]
    (is (= "[1 3]"
           (netlogo/report ws "x-coords-times-charge-k" 4 [[1 2 2] [3 4 2]])))
    (is (= "[1 0.75]"
           (netlogo/report ws "x-coords-times-charge-k" 4 [[1 2 2] [3 4 0 0.25]])))))

(deftest y-coord-times-charge-k
  (let [ws (netlogo/create-workspace model)]
    (is (= "375"
           (netlogo/report ws "y-coord-times-charge-k" 500 [2 3 2])))
    (is (= "0.75"
           (netlogo/report ws "y-coord-times-charge-k" 500 [2 3 0 0.25])))))

(deftest y-coords-times-charge-k
  (let [ws (netlogo/create-workspace model)]
    (is (= "[50 100]"
           (netlogo/report ws "y-coords-times-charge-k" 100 [[1 2 2] [3 4 2]])))
    (is (= "[50 1]"
           (netlogo/report ws "y-coords-times-charge-k" 100 [[1 2 2] [3 4 0 0.25]])))))

(deftest summed-charge-k
  (let [ws (netlogo/create-workspace model)]
    (is (= "2.0011337868480723"
           (netlogo/report ws "summed-charge-k" 10 [[1 1 7]
                                                    [2 2 3]
                                                    [3 3 7]
                                                    [4 4 6]
                                                    [4 4 7]])))
    (is (= "28.47222222222222"
           (netlogo/report ws "summed-charge-k" 20 [[1 0 1]
                                                    [0 2 2]
                                                    [-3 0 3]
                                                    [0 -4 4]])))
    (is (= "10"
           (netlogo/report ws "summed-charge-k" 50 [[1 0 0 1]
                                                    [0 2 0 2]
                                                    [-3 0 0 3]
                                                    [0 -4 0 4]])))))

(deftest center-of-charge-k
  (let [ws (netlogo/create-workspace model)]
    (is (= "[0 0]"
           (netlogo/report ws "center-of-charge-k" 200 [[1 0 1]
                                                        [0 1 1]
                                                        [-1 0 1]
                                                        [0 -1 1]])))
    (is (= "[1.1111111111111112 1.1111111111111112]"
           (netlogo/report ws "center-of-charge-k" 200 [[1 1 1]
                                                        [2 2 (Math/sqrt 8)]])))
    (is (= "[2.307692307692308 2.307692307692308]"
           (netlogo/report ws "center-of-charge-k" 200 [[2 2 (Math/sqrt 8)]
                                                        [3 3 (Math/sqrt 18)]])))
    (is (= "[3.36 3.36]"
           (netlogo/report ws "center-of-charge-k" 200 [[3 3 (Math/sqrt 18)]
                                                        [4 4 (Math/sqrt 32)]])))
    (is (= "[1.2722063037249283 1.2722063037249283]"
           (netlogo/report ws "center-of-charge-k" 200 [[1 1 1]
                                                        [2 2 (Math/sqrt 8)]
                                                        [3 3 (Math/sqrt 18)]
                                                        [4 4 (Math/sqrt 32)]])))
    (is (= "[0.4682926829268293 0.17560975609756097]"
           (netlogo/report ws "center-of-charge-k" 200 [[1 0 1]
                                                        [0 2 2]
                                                        [-3 0 3]
                                                        [0 -4 4]])))))
