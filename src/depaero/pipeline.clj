(ns depaero.pipeline)

(def pipes
  [tagify
   strip-profile?
   apply-refs
   postwalk-remaining-tags])
