include Analysis_DataStructures_Base

module Parser = struct
  module LogicFormulas = LogicFormulas
  module Commands = RegularCommands
end

module Analysis = struct
  include ControlFlowGraph
  module State = State
end