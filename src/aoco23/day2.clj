(ns aoco23.day2
  (:use [clojure.core.logic])
  (:use [clojure.core.logic.dcg])
  (:refer-clojure :exclude [==])
  (:require [clojure.string :as str]
            [clojure.core.logic.fd :as fd]
            [clojure.java.io :as io]))

(def sample-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
(def sample-line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
(def invalid-line "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")


(defn parse-reveal [s]
  (let [[n color] (str/split (str/trim s) #" ")]

    {(keyword color) (parse-long (str/trim n))}))

(defn parse-set [game-set]
  (let [reveals (str/split game-set #",")]
    (->> reveals
         (map #(-> % parse-reveal))
         (apply merge))))

(comment
  (parse-set " 10 red, 3 green"))

(defn parse-line [line]
  (let [[game params] (str/split line #":")
        [_ game-no] (str/split game #" ")
        game-no (parse-long (str/trim game-no))
        param-vec (str/split params #";")]
    [game-no (->> (map parse-set param-vec)
                  (mapv #(let [{:keys [red green blue]} %]
                           [(or red 0)
                            (or green 0)
                            (or blue 0)])))]))

(comment
  (parse-line sample-line)
  ;; => ({:blue 3, :red 4} {:red 1, :green 2, :blue 6} {:green 2})
  )

;; -- WRONG START
;; wrong but keeping it as it's cool
(defne sumo [coll r' g' b']
  ([[] 0 0 0])
  ([[[r g b] . t] _ _ _]
   (fresh [fr fg fb]
     (fd/+ r fr r')
     (fd/+ g fg g')
     (fd/+ b fb b')
     (sumo t fr fg fb))))

(defn check-line [line]
  (first
    (run* [n]
      (let [[game-no params] (parse-line line)]
        (fresh [r g b]
          (== n game-no)
          (fd/>= 12 r)
          (fd/>= 13 g)
          (fd/>= 14 b)
          (sumo params r g b))))))

;; -- WRONG END


(defne valido [line]
  ([[] true])
  ([[[r g b] . t] _]
   (fd/>= 12 r)
   (fd/>= 13 g)
   (fd/>= 14 b)
   (valido t)))

(comment
  (run* [q]
    (valido [[11 8 6] [4 13 5] [1 5 0]])))

(defn check-line2 [line]
  (first
    (let [[game-no params] (parse-line line)]
      (run* [q]
        (== q game-no)
        (valido params)))))

(comment
  (parse-line invalid-line))

(comment
  (check-line2 sample-line))

(def sample-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n")

(comment
  (->> (slurp (io/resource "day2.txt"))
       (str/split-lines)
       (keep check-line2)
       (apply +)))

(comment
  (->> (slurp (io/resource "day2.txt"))
       (str/split-lines)
       (map parse-line)))


;; part 2

(defn my-min [coll]
  (cond
    (nil? (seq coll)) nil
    (= (count coll) 1) (first coll)
    :else (let [[x & r] coll]
            (if (< x (my-min r))
              x
              (my-min r)))))

(comment
  (my-min [10 21 14 5]))

(defna mino [coll q]
  ([[] nil])
  ([[h] h])
  ([[h1 h2 . t] _]
   (conda
     [
      (fd/> h1 h2)
      (mino t h2)
      ]
     [
      (fd/<= h1 h2)
      (mino t h1)
      ]
     )))

(comment
  (run* [q]
    (mino [10 11 ] q)))
