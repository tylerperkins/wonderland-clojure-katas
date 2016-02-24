(ns alphabet-cipher.coder)


(defn encode-char
  "Returns a lower-case cipher letter that is the given lower-case letter
  c encoded by the given lower-case keyword letter key-char.
  "
  [key-char c]
  (-> (+ -194 (int key-char) (int c))       ; Int result: 0 thru 50
      (mod 26)                              ; 0 thru 25  (\a thru \z)
      (+ 97)                                ; 97 thru 122
      char))                                ; \a thru \z


(defn decode-char
  "For a given lower-case key letter, this function is the inverse of
  encode-char. Given the (lower-case) keyword letter and a cipher
  (lower-case) letter, this function returns the original (lower-case)
  letter.
  "
  [key-char encoded-char]
  (-> (- (int encoded-char) (int key-char))
      (mod 26)
      (+ 97)
      char))


(defn min-seed
  "Returns the shortest prefix p of the given string s such that s is a
  prefix of (cycle p).
  "
  [s]
  (apply str                                ; Turn seq of chars to String.

         (loop [seed      (vec (take 1 s))  ; [] if s is "",
                                            ;  or [\x], if s is "xyz"
                seed-rest (rest s)
                ]
           (if (every? identity (map = (cycle seed) seed-rest))
             seed                           ; Success!
                                            ; Else, try longer seed.
             (recur (conj seed (first seed-rest))
                    (rest seed-rest))))))


(defn encode
  "Described in README.md. Note that keyword, message, and the resulting
  cipher must all be lower-case.
  "
  [keyword message]
  (apply str (map encode-char (cycle keyword) message)))


(defn decode
  "Described in README.md. Note that keyword, cipher, and the resulting
  message must all be lower-case.
  "
  [keyword cipher]
  (apply str (map decode-char (cycle keyword) cipher)))


(defn decipher
  "Returns a keyword such that (encode keyword message) results in
  the given cipher. The shortest possible such keyword is returned.
  E.g., if cipher is the result of (encode \"foofoo\" message),
  then (decipher cipher message) would return \"foo\".
  "
  [cipher message]
  (min-seed (decode message cipher)))

