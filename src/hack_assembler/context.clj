(ns hack-assembler.context)

(defn initialize-context
    "Initialize a context with line and instruction numbers 1 and
    the supplied symbol table"
    [table]
    {:line-number 1
     :instruction-number 1
     :symbol-table table})

(defn inc-instruction
    "Increments the context's instruction-number"
    [context]
    (update context :instruction-number inc))

(defn inc-line
    "Increments the context's line-number"
    [context]
    (update context :line-number inc))

(defn update-table
    "Replaces the context's symbol table with the specified table"
    [context table]
    (conj context [:symbol-table table]))
    
