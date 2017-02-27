(ns hack-assembler.context
    (:require [hack-assembler.symbol-table :as symt]))

(defn initialize-context
    "Initialize a context with line and instruction numbers 1 and a new symbol table"
    []
    {:line-number 1
     :instruction-number 1
     :symbol-table (symt/initialize-table)})

(defn inc-instruction
    "Increments the context's instruction-number"
    [context]
    (update context :instruction-number inc))

(defn inc-line
    "Increments the context's line-number"
    [context]
    (update context :line-number inc))

(defn update-table
    "Updates the context's symbol table by applying f to the 
    current table and any other supplied args"
    [{table :symbol-table :as context} f & args]
    (conj context [:symbol-table (apply f (cons table args))]))
