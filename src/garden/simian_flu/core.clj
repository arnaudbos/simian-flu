(ns simian-flu.core
  (:require [garden.def :refer [defstylesheet defstyles]]
            [garden.units :refer [px]]))

(defstyles ui
           [:.cell
            {:-fx {:background-color "white"}}]
           [:.human
            {:-fx {}}]
           (for [[state color] '(["sane" "lime"] ["infected" "red"] ["immune" "cyan"])]
             [(keyword (str ".human-" state))
              {:-fx {:fill color}}])
           [:.ape
            {:-fx {:fill "darkred"}}])
