(ns alphabet-cipher.coder-test
  (:require [clojure.test :refer :all]
            [alphabet-cipher.coder :refer :all]))


(deftest test-encode-char
  (are [key-char  c     result] (= result (encode-char key-char c))
        \a        \a    \a
        \a        \z    \z
        \z        \a    \z
        \z        \z    \y
        \h        \f    \m
        \f        \h    \m
    ))


(deftest test-decode-char
  (are [key-char  c     result] (= result (decode-char key-char c))
        \a        \a    \a
        \a        \z    \z
        \z        \z    \a
        \z        \y    \z
        \h        \m    \f
        \f        \m    \h

    ))


(deftest test-min-seed
  (are [s             result] (= result (min-seed s))
        ""            ""
        "x"           "x"
        "xxxxxxxxxxx" "x"
        "xyzxyzxyzxy" "xyz"
        "xyxxyxxyxxy" "xyx"
        "xxyxxyxxyxx" "xxy"
        "xyzxyzxyzzx" "xyzxyzxyzz"
    ))


(deftest test-encode
  (testing "can encode given a secret keyword"
    (is (= "hmkbxebpxpmyllyrxiiqtoltfgzzv"
           (encode "vigilance" "meetmeontuesdayeveningatseven")))
    (is (= "egsgqwtahuiljgs"
           (encode "scones" "meetmebythetree")))))


(deftest test-decode
  (testing "can decode an cyrpted message given a secret keyword"
    (is (= "meetmeontuesdayeveningatseven"
           (decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv")))
    (is (= "meetmebythetree"
           (decode "scones" "egsgqwtahuiljgs")))))


(deftest test-decipher
  (testing "can extract the secret keyword given an encrypted message and the original message"
    (is (= "vigilance"
           (decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog")))
    (is (= "scones"
           (decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs")))))

