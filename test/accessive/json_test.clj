(ns accessive.json-test
  (:require [accessive.json :refer :all]
            [clojure.test :refer :all]))


(def test-json-str
  " {
     \"foo\" :
     10 ,

     \"11\":[ true, {\"bar\": [ \"a\\\\\\\"a\", 12, \"bb\", [ x,\"y\" ] ] ,
              \"ba z\" : \"qux\"
             }
     ,
     \"quux\"
     ],\" plugh\" :\" xyzzy \" ,
     \"Fo\\\"o\" : 13 
    }
  ")

(deftest get-in-json-test
  (are [ks v] (= (get-in-json test-json-str ks) v)
       ["foo"] "10"
       [:foo] "10"
       ["bar"] nil
       ["11" 0] "true"
       ["11" 1 "ba z"] "\"qux\""
       ["11" 1 "bar" 0] "\"a\\\\\\\"a\""
       ["11" 1 "bar" 3] "[ x,\"y\" ]"
       ["11" 1 "bar" 3 0] "x"
       ["11" 1 "bar" 3 1] "\"y\""
       ["11" 2] "\"quux\""
       ["11" 3] nil
       [" plugh"] "\" xyzzy \""
       ["plugh"] nil
       [:plugh] nil
       ["Fo\"o"] "13"
       ))

(deftest get-in-json-not-found-test
  (are [ks v] (= (get-in-json test-json-str ks 1) v)
       ["bar"] 1
       ["11" 3] 1
       ["plugh"] 1
       [:plugh] 1
       ))

(deftest get-tree-in-json-test
  (let [in-tree {"foo" ""
                 "bar" nil
                 "11" {0 nil
                       1 {:bar {1 nil
                                4 nil
                                3 {1 ""}}}}
                 "Fo\"o" nil}
        out-tree {"foo" 10
                  "bar" nil
                  "11" {0 true
                        1 {:bar {1 12
                                 4 nil
                                 3 {1 "y"}}}}
                  "Fo\"o" 13}]
    (is (= out-tree
           (get-tree-in-json test-json-str in-tree read-string)))
    (is (= out-tree
           (get-lazy-tree-in-json test-json-str in-tree read-string)))))

(deftest get-tree-in-json-not-found-test
  (let [in-tree {:bar {0 nil}}
        lazy-out-tree {:bar {0 nil}}
        eager-out-tree {:bar nil}]
    (is (= eager-out-tree
           (get-tree-in-json test-json-str in-tree)))
    (is (= lazy-out-tree
           (get-lazy-tree-in-json test-json-str in-tree)))))
