(ns hack-assembler.symbol-table)

(defn initialize-table
 "Initializes a symbol table map with buil-in symbols
 and sets current var address to 0"
 []
 {:cur-var-address 0x0010
  "SP" 0x0000
  "LCL" 0x0001
  "ARG" 0x0002
  "THIS" 0x0003
  "THAT" 0x0004
  "R0" 0x0000
  "R1" 0x0001
  "R2" 0x0002
  "R3" 0x0003
  "R4" 0x0004
  "R5" 0x0005
  "R6" 0x0006
  "R7" 0x0007
  "R8" 0x0008
  "R9" 0x0009
  "R10" 0x000A
  "R11" 0x000B
  "R12" 0x000C
  "R13" 0x000D
  "R14" 0x000E
  "R15" 0x000F
  "SCREEN" 0x4000
  "KBD" 0x6000})

(defn add-variable
 "Adds a variable to the table at the table's current var address"
 [table sym]
 (let [address (:cur-var-address table)
       new-table (conj table [sym address])]
      (update new-table :cur-var-address inc)))

(defn resolve-variable
 "Looks for sym in table, if found returns a pair of table and the value of sym.
 If not found, adds a variable with the name sym to table and returns the updated
 table and the value of the added variable"
 [table sym]
 (if-let [val (get table sym)]
     [table val]
     (let [new-table (add-variable table sym)
           val (get new-table sym)]
          [new-table val])))
         
(defn resolve-symbol
 "Resolves the address value of sym and returns a vector of the
 table and resolved value"
 [table sym]
 (if-let [val (get table sym)]
     [table val]
     (resolve-variable table sym)))
