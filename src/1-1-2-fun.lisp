(in-package :cl-cudd.baseapi)

(defcstruct #.(lispify "DdChildren" 'classname)
            (t :pointer)
            (e :pointer))

(defcstruct #.(lispify "DdNode" 'classname)
            (index :unsigned-short)
            (ref :unsigned-short)
            (next node)
            (type :pointer))

(defcunion #.(lispify "DdNode_type" 'classname)
           (value :double)
           (kids (:struct dd-children)))

(defcfun ("Cudd_addNewVar" #.(lispify "Cudd_addNewVar" :function)) node
  (dd manager))
(defcfun ("Cudd_addNewVarAtLevel" #.(lispify "Cudd_addNewVarAtLevel" :function)) node
  (dd manager)
  (level :int))
(defcfun ("Cudd_bddNewVar" #.(lispify "Cudd_bddNewVar" :function)) node
  (dd manager))
(defcfun ("Cudd_bddNewVarAtLevel" #.(lispify "Cudd_bddNewVarAtLevel" :function)) node
  (dd manager)
  (level :int))
(defcfun ("Cudd_addIthVar" #.(lispify "Cudd_addIthVar" :function)) node
  (dd manager)
  (i :int))
(defcfun ("Cudd_bddIthVar" #.(lispify "Cudd_bddIthVar" :function)) node
  (dd manager)
  (i :int))
(defcfun ("Cudd_zddIthVar" #.(lispify "Cudd_zddIthVar" :function)) node
  (dd manager)
  (i :int))
(defcfun ("Cudd_zddVarsFromBddVars" #.(lispify "Cudd_zddVarsFromBddVars" :function)) :int
  (dd manager)
  (multiplicity :int))
(defcfun ("Cudd_addConst" #.(lispify "Cudd_addConst" :function)) node
  (dd manager)
  (c :double))
(defcfun ("Cudd_IsNonConstant" #.(lispify "Cudd_IsNonConstant" :function)) :int
  (f node))
(defcfun ("Cudd_AutodynEnable" #.(lispify "Cudd_AutodynEnable" :function)) :void
  (unique manager)
  (method #.(lispify "Cudd_ReorderingType" :enumname)))
(defcfun ("Cudd_AutodynDisable" #.(lispify "Cudd_AutodynDisable" :function)) :void
  (unique manager))
(defcfun ("Cudd_ReorderingStatus" #.(lispify "Cudd_ReorderingStatus" :function)) :int
  (unique manager)
  (method :pointer))
(defcfun ("Cudd_AutodynEnableZdd" #.(lispify "Cudd_AutodynEnableZdd" :function)) :void
  (unique manager)
  (method #.(lispify "Cudd_ReorderingType" :enumname)))
(defcfun ("Cudd_AutodynDisableZdd" #.(lispify "Cudd_AutodynDisableZdd" :function)) :void
  (unique manager))
(defcfun ("Cudd_ReorderingStatusZdd" #.(lispify "Cudd_ReorderingStatusZdd" :function)) :int
  (unique manager)
  (method :pointer))
(defcfun ("Cudd_zddRealignmentEnabled" #.(lispify "Cudd_zddRealignmentEnabled" :function)) :int
  (unique manager))
(defcfun ("Cudd_zddRealignEnable" #.(lispify "Cudd_zddRealignEnable" :function)) :void
  (unique manager))
(defcfun ("Cudd_zddRealignDisable" #.(lispify "Cudd_zddRealignDisable" :function)) :void
  (unique manager))
(defcfun ("Cudd_bddRealignmentEnabled" #.(lispify "Cudd_bddRealignmentEnabled" :function)) :int
  (unique manager))
(defcfun ("Cudd_bddRealignEnable" #.(lispify "Cudd_bddRealignEnable" :function)) :void
  (unique manager))
(defcfun ("Cudd_bddRealignDisable" #.(lispify "Cudd_bddRealignDisable" :function)) :void
  (unique manager))
(defcfun ("Cudd_ReadOne" #.(lispify "Cudd_ReadOne" :function)) node
  (dd manager))
(defcfun ("Cudd_ReadZddOne" #.(lispify "Cudd_ReadZddOne" :function)) node
  (dd manager)
  (i :int))
(defcfun ("Cudd_ReadZero" #.(lispify "Cudd_ReadZero" :function)) node
  (dd manager))
(defcfun ("Cudd_ReadLogicZero" #.(lispify "Cudd_ReadLogicZero" :function)) node
  (dd manager))
(defcfun ("Cudd_ReadPlusInfinity" #.(lispify "Cudd_ReadPlusInfinity" :function)) node
  (dd manager))
(defcfun ("Cudd_ReadMinusInfinity" #.(lispify "Cudd_ReadMinusInfinity" :function)) node
  (dd manager))
(defcfun ("Cudd_ReadBackground" #.(lispify "Cudd_ReadBackground" :function)) node
  (dd manager))
(defcfun ("Cudd_SetBackground" #.(lispify "Cudd_SetBackground" :function)) :void
  (dd manager)
  (bck node))
(defcfun ("Cudd_ReadCacheSlots" #.(lispify "Cudd_ReadCacheSlots" :function)) :unsigned-int
  (dd manager))
(defcfun ("Cudd_ReadCacheUsedSlots" #.(lispify "Cudd_ReadCacheUsedSlots" :function)) :double
  (dd manager))
(defcfun ("Cudd_ReadCacheLookUps" #.(lispify "Cudd_ReadCacheLookUps" :function)) :double
  (dd manager))
(defcfun ("Cudd_ReadCacheHits" #.(lispify "Cudd_ReadCacheHits" :function)) :double
  (dd manager))
(defcfun ("Cudd_ReadRecursiveCalls" #.(lispify "Cudd_ReadRecursiveCalls" :function)) :double
  (dd manager))
(defcfun ("Cudd_ReadMinHit" #.(lispify "Cudd_ReadMinHit" :function)) :unsigned-int
  (dd manager))
(defcfun ("Cudd_SetMinHit" #.(lispify "Cudd_SetMinHit" :function)) :void
  (dd manager)
  (hr :unsigned-int))
(defcfun ("Cudd_ReadLooseUpTo" #.(lispify "Cudd_ReadLooseUpTo" :function)) :unsigned-int
  (dd manager))
(defcfun ("Cudd_SetLooseUpTo" #.(lispify "Cudd_SetLooseUpTo" :function)) :void
  (dd manager)
  (lut :unsigned-int))
(defcfun ("Cudd_ReadMaxCache" #.(lispify "Cudd_ReadMaxCache" :function)) :unsigned-int
  (dd manager))
(defcfun ("Cudd_ReadMaxCacheHard" #.(lispify "Cudd_ReadMaxCacheHard" :function)) :unsigned-int
  (dd manager))
(defcfun ("Cudd_SetMaxCacheHard" #.(lispify "Cudd_SetMaxCacheHard" :function)) :void
  (dd manager)
  (mc :unsigned-int))
(defcfun ("Cudd_ReadSize" #.(lispify "Cudd_ReadSize" :function)) :int
  (dd manager))
(defcfun ("Cudd_ReadZddSize" #.(lispify "Cudd_ReadZddSize" :function)) :int
  (dd manager))
(defcfun ("Cudd_ReadSlots" #.(lispify "Cudd_ReadSlots" :function)) :unsigned-int
  (dd manager))
(defcfun ("Cudd_ReadUsedSlots" #.(lispify "Cudd_ReadUsedSlots" :function)) :double
  (dd manager))
(defcfun ("Cudd_ExpectedUsedSlots" #.(lispify "Cudd_ExpectedUsedSlots" :function)) :double
  (dd manager))
(defcfun ("Cudd_ReadKeys" #.(lispify "Cudd_ReadKeys" :function)) :unsigned-int
  (dd manager))
(defcfun ("Cudd_ReadDead" #.(lispify "Cudd_ReadDead" :function)) :unsigned-int
  (dd manager))
(defcfun ("Cudd_ReadMinDead" #.(lispify "Cudd_ReadMinDead" :function)) :unsigned-int
  (dd manager))
(defcfun ("Cudd_ReadReorderings" #.(lispify "Cudd_ReadReorderings" :function)) :int
  (dd manager))
(defcfun ("Cudd_ReadReorderingTime" #.(lispify "Cudd_ReadReorderingTime" :function)) :long
  (dd manager))
(defcfun ("Cudd_ReadGarbageCollections" #.(lispify "Cudd_ReadGarbageCollections" :function)) :int
  (dd manager))
(defcfun ("Cudd_ReadGarbageCollectionTime" #.(lispify "Cudd_ReadGarbageCollectionTime" :function)) :long
  (dd manager))
(defcfun ("Cudd_ReadNodesFreed" #.(lispify "Cudd_ReadNodesFreed" :function)) :double
  (dd manager))
(defcfun ("Cudd_ReadNodesDropped" #.(lispify "Cudd_ReadNodesDropped" :function)) :double
  (dd manager))
(defcfun ("Cudd_ReadUniqueLookUps" #.(lispify "Cudd_ReadUniqueLookUps" :function)) :double
  (dd manager))
(defcfun ("Cudd_ReadUniqueLinks" #.(lispify "Cudd_ReadUniqueLinks" :function)) :double
  (dd manager))
(defcfun ("Cudd_ReadSiftMaxVar" #.(lispify "Cudd_ReadSiftMaxVar" :function)) :int
  (dd manager))
(defcfun ("Cudd_SetSiftMaxVar" #.(lispify "Cudd_SetSiftMaxVar" :function)) :void
  (dd manager)
  (smv :int))
(defcfun ("Cudd_ReadSiftMaxSwap" #.(lispify "Cudd_ReadSiftMaxSwap" :function)) :int
  (dd manager))
(defcfun ("Cudd_SetSiftMaxSwap" #.(lispify "Cudd_SetSiftMaxSwap" :function)) :void
  (dd manager)
  (sms :int))
(defcfun ("Cudd_ReadMaxGrowth" #.(lispify "Cudd_ReadMaxGrowth" :function)) :double
  (dd manager))
(defcfun ("Cudd_SetMaxGrowth" #.(lispify "Cudd_SetMaxGrowth" :function)) :void
  (dd manager)
  (mg :double))
(defcfun ("Cudd_ReadMaxGrowthAlternate" #.(lispify "Cudd_ReadMaxGrowthAlternate" :function)) :double
  (dd manager))
(defcfun ("Cudd_SetMaxGrowthAlternate" #.(lispify "Cudd_SetMaxGrowthAlternate" :function)) :void
  (dd manager)
  (mg :double))
(defcfun ("Cudd_ReadReorderingCycle" #.(lispify "Cudd_ReadReorderingCycle" :function)) :int
  (dd manager))
(defcfun ("Cudd_SetReorderingCycle" #.(lispify "Cudd_SetReorderingCycle" :function)) :void
  (dd manager)
  (cycle :int))
(defcfun ("Cudd_ReadTree" #.(lispify "Cudd_ReadTree" :function)) :pointer
  (dd manager))
(defcfun ("Cudd_SetTree" #.(lispify "Cudd_SetTree" :function)) :void
  (dd manager)
  (tree :pointer))
(defcfun ("Cudd_FreeTree" #.(lispify "Cudd_FreeTree" :function)) :void
  (dd manager))
(defcfun ("Cudd_ReadZddTree" #.(lispify "Cudd_ReadZddTree" :function)) :pointer
  (dd manager))
(defcfun ("Cudd_SetZddTree" #.(lispify "Cudd_SetZddTree" :function)) :void
  (dd manager)
  (tree :pointer))
(defcfun ("Cudd_FreeZddTree" #.(lispify "Cudd_FreeZddTree" :function)) :void
  (dd manager))
(defcfun ("Cudd_NodeReadIndex" #.(lispify "Cudd_NodeReadIndex" :function)) :unsigned-int
  (node node))
(defcfun ("Cudd_ReadPerm" #.(lispify "Cudd_ReadPerm" :function)) :int
  (dd manager)
  (i :int))
(defcfun ("Cudd_ReadPermZdd" #.(lispify "Cudd_ReadPermZdd" :function)) :int
  (dd manager)
  (i :int))
(defcfun ("Cudd_ReadInvPerm" #.(lispify "Cudd_ReadInvPerm" :function)) :int
  (dd manager)
  (i :int))
(defcfun ("Cudd_ReadInvPermZdd" #.(lispify "Cudd_ReadInvPermZdd" :function)) :int
  (dd manager)
  (i :int))
(defcfun ("Cudd_ReadVars" #.(lispify "Cudd_ReadVars" :function)) node
  (dd manager)
  (i :int))
(defcfun ("Cudd_ReadEpsilon" #.(lispify "Cudd_ReadEpsilon" :function)) :double
  (dd manager))
(defcfun ("Cudd_SetEpsilon" #.(lispify "Cudd_SetEpsilon" :function)) :void
  (dd manager)
  (ep :double))
(defcfun ("Cudd_ReadGroupcheck" #.(lispify "Cudd_ReadGroupcheck" :function)) #.(lispify "Cudd_AggregationType" :enumname)
  (dd manager))
(defcfun ("Cudd_SetGroupcheck" #.(lispify "Cudd_SetGroupcheck" :function)) :void
  (dd manager)
  (gc #.(lispify "Cudd_AggregationType" :enumname)))
(defcfun ("Cudd_GarbageCollectionEnabled" #.(lispify "Cudd_GarbageCollectionEnabled" :function)) :int
  (dd manager))
(defcfun ("Cudd_EnableGarbageCollection" #.(lispify "Cudd_EnableGarbageCollection" :function)) :void
  (dd manager))
(defcfun ("Cudd_DisableGarbageCollection" #.(lispify "Cudd_DisableGarbageCollection" :function)) :void
  (dd manager))
(defcfun ("Cudd_DeadAreCounted" #.(lispify "Cudd_DeadAreCounted" :function)) :int
  (dd manager))
(defcfun ("Cudd_TurnOnCountDead" #.(lispify "Cudd_TurnOnCountDead" :function)) :void
  (dd manager))
(defcfun ("Cudd_TurnOffCountDead" #.(lispify "Cudd_TurnOffCountDead" :function)) :void
  (dd manager))
(defcfun ("Cudd_ReadRecomb" #.(lispify "Cudd_ReadRecomb" :function)) :int
  (dd manager))
(defcfun ("Cudd_SetRecomb" #.(lispify "Cudd_SetRecomb" :function)) :void
  (dd manager)
  (recomb :int))
(defcfun ("Cudd_ReadSymmviolation" #.(lispify "Cudd_ReadSymmviolation" :function)) :int
  (dd manager))
(defcfun ("Cudd_SetSymmviolation" #.(lispify "Cudd_SetSymmviolation" :function)) :void
  (dd manager)
  (symmviolation :int))
(defcfun ("Cudd_ReadArcviolation" #.(lispify "Cudd_ReadArcviolation" :function)) :int
  (dd manager))
(defcfun ("Cudd_SetArcviolation" #.(lispify "Cudd_SetArcviolation" :function)) :void
  (dd manager)
  (arcviolation :int))
(defcfun ("Cudd_ReadPopulationSize" #.(lispify "Cudd_ReadPopulationSize" :function)) :int
  (dd manager))
(defcfun ("Cudd_SetPopulationSize" #.(lispify "Cudd_SetPopulationSize" :function)) :void
  (dd manager)
  (populationSize :int))
(defcfun ("Cudd_ReadNumberXovers" #.(lispify "Cudd_ReadNumberXovers" :function)) :int
  (dd manager))
(defcfun ("Cudd_SetNumberXovers" #.(lispify "Cudd_SetNumberXovers" :function)) :void
  (dd manager)
  (numberXovers :int))
(defcfun ("Cudd_ReadMemoryInUse" #.(lispify "Cudd_ReadMemoryInUse" :function)) :unsigned-long
  (dd manager))
(defcfun ("Cudd_PrintInfo" #.(lispify "Cudd_PrintInfo" :function)) :int
  (dd manager)
  (fp :pointer))
(defcfun ("Cudd_ReadPeakNodeCount" #.(lispify "Cudd_ReadPeakNodeCount" :function)) :long
  (dd manager))
(defcfun ("Cudd_ReadPeakLiveNodeCount" #.(lispify "Cudd_ReadPeakLiveNodeCount" :function)) :int
  (dd manager))
(defcfun ("Cudd_ReadNodeCount" #.(lispify "Cudd_ReadNodeCount" :function)) :long
  (dd manager))
(defcfun ("Cudd_zddReadNodeCount" #.(lispify "Cudd_zddReadNodeCount" :function)) :long
  (dd manager))
(defcfun ("Cudd_AddHook" #.(lispify "Cudd_AddHook" :function)) :int
  (dd manager)
  (f :pointer)
  (where #.(lispify "Cudd_HookType" :enumname)))
(defcfun ("Cudd_RemoveHook" #.(lispify "Cudd_RemoveHook" :function)) :int
  (dd manager)
  (f :pointer)
  (where #.(lispify "Cudd_HookType" :enumname)))
(defcfun ("Cudd_IsInHook" #.(lispify "Cudd_IsInHook" :function)) :int
  (dd manager)
  (f :pointer)
  (where #.(lispify "Cudd_HookType" :enumname)))
(defcfun ("Cudd_StdPreReordHook" #.(lispify "Cudd_StdPreReordHook" :function)) :int
  (dd manager)
  (str :string)
  (data :pointer))
(defcfun ("Cudd_StdPostReordHook" #.(lispify "Cudd_StdPostReordHook" :function)) :int
  (dd manager)
  (str :string)
  (data :pointer))
(defcfun ("Cudd_EnableReorderingReporting" #.(lispify "Cudd_EnableReorderingReporting" :function)) :int
  (dd manager))
(defcfun ("Cudd_DisableReorderingReporting" #.(lispify "Cudd_DisableReorderingReporting" :function)) :int
  (dd manager))
(defcfun ("Cudd_ReorderingReporting" #.(lispify "Cudd_ReorderingReporting" :function)) :int
  (dd manager))
(defcfun ("Cudd_ReadErrorCode" #.(lispify "Cudd_ReadErrorCode" :function)) #.(lispify "Cudd_ErrorType" :enumname)
  (dd manager))
(defcfun ("Cudd_ClearErrorCode" #.(lispify "Cudd_ClearErrorCode" :function)) :void
  (dd manager))
(defcfun ("Cudd_ReadStdout" #.(lispify "Cudd_ReadStdout" :function)) :pointer
  (dd manager))
(defcfun ("Cudd_SetStdout" #.(lispify "Cudd_SetStdout" :function)) :void
  (dd manager)
  (fp :pointer))
(defcfun ("Cudd_ReadStderr" #.(lispify "Cudd_ReadStderr" :function)) :pointer
  (dd manager))
(defcfun ("Cudd_SetStderr" #.(lispify "Cudd_SetStderr" :function)) :void
  (dd manager)
  (fp :pointer))
(defcfun ("Cudd_ReadNextReordering" #.(lispify "Cudd_ReadNextReordering" :function)) :unsigned-int
  (dd manager))
(defcfun ("Cudd_SetNextReordering" #.(lispify "Cudd_SetNextReordering" :function)) :void
  (dd manager)
  (next :unsigned-int))
(defcfun ("Cudd_ReadSwapSteps" #.(lispify "Cudd_ReadSwapSteps" :function)) :double
  (dd manager))
(defcfun ("Cudd_ReadMaxLive" #.(lispify "Cudd_ReadMaxLive" :function)) :unsigned-int
  (dd manager))
(defcfun ("Cudd_SetMaxLive" #.(lispify "Cudd_SetMaxLive" :function)) :void
  (dd manager)
  (maxLive :unsigned-int))
(defcfun ("Cudd_ReadMaxMemory" #.(lispify "Cudd_ReadMaxMemory" :function)) :unsigned-long
  (dd manager))
(defcfun ("Cudd_SetMaxMemory" #.(lispify "Cudd_SetMaxMemory" :function)) :void
  (dd manager)
  (maxMemory :unsigned-long))
(defcfun ("Cudd_bddBindVar" #.(lispify "Cudd_bddBindVar" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddUnbindVar" #.(lispify "Cudd_bddUnbindVar" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddVarIsBound" #.(lispify "Cudd_bddVarIsBound" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_addExistAbstract" #.(lispify "Cudd_addExistAbstract" :function)) node
  (manager manager)
  (f node)
  (cube node))
(defcfun ("Cudd_addUnivAbstract" #.(lispify "Cudd_addUnivAbstract" :function)) node
  (manager manager)
  (f node)
  (cube node))
(defcfun ("Cudd_addOrAbstract" #.(lispify "Cudd_addOrAbstract" :function)) node
  (manager manager)
  (f node)
  (cube node))
(defcfun ("Cudd_addApply" #.(lispify "Cudd_addApply" :function)) node
  (dd manager)
  (arg1 :pointer)
  (f node)
  (g node))
(defcfun ("Cudd_addPlus" #.(lispify "Cudd_addPlus" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addTimes" #.(lispify "Cudd_addTimes" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addThreshold" #.(lispify "Cudd_addThreshold" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addSetNZ" #.(lispify "Cudd_addSetNZ" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addDivide" #.(lispify "Cudd_addDivide" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addMinus" #.(lispify "Cudd_addMinus" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addMinimum" #.(lispify "Cudd_addMinimum" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addMaximum" #.(lispify "Cudd_addMaximum" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addOneZeroMaximum" #.(lispify "Cudd_addOneZeroMaximum" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addDiff" #.(lispify "Cudd_addDiff" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addAgreement" #.(lispify "Cudd_addAgreement" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addOr" #.(lispify "Cudd_addOr" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addNand" #.(lispify "Cudd_addNand" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addNor" #.(lispify "Cudd_addNor" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addXor" #.(lispify "Cudd_addXor" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addXnor" #.(lispify "Cudd_addXnor" :function)) node
  (dd manager)
  (f :pointer)
  (g :pointer))
(defcfun ("Cudd_addMonadicApply" #.(lispify "Cudd_addMonadicApply" :function)) node
  (dd manager)
  (op :pointer)
  (f node))
(defcfun ("Cudd_addLog" #.(lispify "Cudd_addLog" :function)) node
  (dd manager)
  (f node))
(defcfun ("Cudd_addFindMax" #.(lispify "Cudd_addFindMax" :function)) node
  (dd manager)
  (f node))
(defcfun ("Cudd_addFindMin" #.(lispify "Cudd_addFindMin" :function)) node
  (dd manager)
  (f node))
(defcfun ("Cudd_addIthBit" #.(lispify "Cudd_addIthBit" :function)) node
  (dd manager)
  (f node)
  (bit :int))
(defcfun ("Cudd_addScalarInverse" #.(lispify "Cudd_addScalarInverse" :function)) node
  (dd manager)
  (f node)
  (epsilon node))
(defcfun ("Cudd_addIte" #.(lispify "Cudd_addIte" :function)) node
  (dd manager)
  (f node)
  (g node)
  (h node))
(defcfun ("Cudd_addIteConstant" #.(lispify "Cudd_addIteConstant" :function)) node
  (dd manager)
  (f node)
  (g node)
  (h node))
(defcfun ("Cudd_addEvalConst" #.(lispify "Cudd_addEvalConst" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_addLeq" #.(lispify "Cudd_addLeq" :function)) :int
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_addCmpl" #.(lispify "Cudd_addCmpl" :function)) node
  (dd manager)
  (f node))
(defcfun ("Cudd_addNegate" #.(lispify "Cudd_addNegate" :function)) node
  (dd manager)
  (f node))
(defcfun ("Cudd_addRoundOff" #.(lispify "Cudd_addRoundOff" :function)) node
  (dd manager)
  (f node)
  (N :int))
(defcfun ("Cudd_addWalsh" #.(lispify "Cudd_addWalsh" :function)) node
  (dd manager)
  (x :pointer)
  (y :pointer)
  (n :int))
(defcfun ("Cudd_addResidue" #.(lispify "Cudd_addResidue" :function)) node
  (dd manager)
  (n :int)
  (m :int)
  (options :int)
  (top :int))
(defcfun ("Cudd_bddAndAbstract" #.(lispify "Cudd_bddAndAbstract" :function)) node
  (manager manager)
  (f node)
  (g node)
  (cube node))
(defcfun ("Cudd_bddAndAbstractLimit" #.(lispify "Cudd_bddAndAbstractLimit" :function)) node
  (manager manager)
  (f node)
  (g node)
  (cube node)
  (limit :unsigned-int))
(defcfun ("Cudd_ApaNumberOfDigits" #.(lispify "Cudd_ApaNumberOfDigits" :function)) :int
  (binaryDigits :int))
(defcfun ("Cudd_NewApaNumber" #.(lispify "Cudd_NewApaNumber" :function)) :pointer
  (digits :int))
(defcfun ("Cudd_ApaCopy" #.(lispify "Cudd_ApaCopy" :function)) :void
  (digits :int)
  (source :pointer)
  (dest :pointer))
(defcfun ("Cudd_ApaAdd" #.(lispify "Cudd_ApaAdd" :function)) :unsigned-short
  (digits :int)
  (a :pointer)
  (b :pointer)
  (sum :pointer))
(defcfun ("Cudd_ApaSubtract" #.(lispify "Cudd_ApaSubtract" :function)) :unsigned-short
  (digits :int)
  (a :pointer)
  (b :pointer)
  (diff :pointer))
(defcfun ("Cudd_ApaShortDivision" #.(lispify "Cudd_ApaShortDivision" :function)) :unsigned-short
  (digits :int)
  (dividend :pointer)
  (divisor :unsigned-short)
  (quotient :pointer))
(defcfun ("Cudd_ApaIntDivision" #.(lispify "Cudd_ApaIntDivision" :function)) :unsigned-int
  (digits :int)
  (dividend :pointer)
  (divisor :unsigned-int)
  (quotient :pointer))
(defcfun ("Cudd_ApaShiftRight" #.(lispify "Cudd_ApaShiftRight" :function)) :void
  (digits :int)
  (in :unsigned-short)
  (a :pointer)
  (b :pointer))
(defcfun ("Cudd_ApaSetToLiteral" #.(lispify "Cudd_ApaSetToLiteral" :function)) :void
  (digits :int)
  (number :pointer)
  (literal :unsigned-short))
(defcfun ("Cudd_ApaPowerOfTwo" #.(lispify "Cudd_ApaPowerOfTwo" :function)) :void
  (digits :int)
  (number :pointer)
  (power :int))
(defcfun ("Cudd_ApaCompare" #.(lispify "Cudd_ApaCompare" :function)) :int
  (digitsFirst :int)
  (first :pointer)
  (digitsSecond :int)
  (second :pointer))
(defcfun ("Cudd_ApaCompareRatios" #.(lispify "Cudd_ApaCompareRatios" :function)) :int
  (digitsFirst :int)
  (firstNum :pointer)
  (firstDen :unsigned-int)
  (digitsSecond :int)
  (secondNum :pointer)
  (secondDen :unsigned-int))
(defcfun ("Cudd_ApaPrintHex" #.(lispify "Cudd_ApaPrintHex" :function)) :int
  (fp :pointer)
  (digits :int)
  (number :pointer))
(defcfun ("Cudd_ApaPrintDecimal" #.(lispify "Cudd_ApaPrintDecimal" :function)) :int
  (fp :pointer)
  (digits :int)
  (number :pointer))
(defcfun ("Cudd_ApaPrintExponential" #.(lispify "Cudd_ApaPrintExponential" :function)) :int
  (fp :pointer)
  (digits :int)
  (number :pointer)
  (precision :int))
(defcfun ("Cudd_ApaCountMinterm" #.(lispify "Cudd_ApaCountMinterm" :function)) :pointer
  (manager manager)
  (node node)
  (nvars :int)
  (digits :pointer))
(defcfun ("Cudd_ApaPrintMinterm" #.(lispify "Cudd_ApaPrintMinterm" :function)) :int
  (fp :pointer)
  (dd manager)
  (node node)
  (nvars :int))
(defcfun ("Cudd_ApaPrintMintermExp" #.(lispify "Cudd_ApaPrintMintermExp" :function)) :int
  (fp :pointer)
  (dd manager)
  (node node)
  (nvars :int)
  (precision :int))
(defcfun ("Cudd_ApaPrintDensity" #.(lispify "Cudd_ApaPrintDensity" :function)) :int
  (fp :pointer)
  (dd manager)
  (node node)
  (nvars :int))
(defcfun ("Cudd_UnderApprox" #.(lispify "Cudd_UnderApprox" :function)) node
  (dd manager)
  (f node)
  (numVars :int)
  (threshold :int)
  (safe :int)
  (quality :double))
(defcfun ("Cudd_OverApprox" #.(lispify "Cudd_OverApprox" :function)) node
  (dd manager)
  (f node)
  (numVars :int)
  (threshold :int)
  (safe :int)
  (quality :double))
(defcfun ("Cudd_RemapUnderApprox" #.(lispify "Cudd_RemapUnderApprox" :function)) node
  (dd manager)
  (f node)
  (numVars :int)
  (threshold :int)
  (quality :double))
(defcfun ("Cudd_RemapOverApprox" #.(lispify "Cudd_RemapOverApprox" :function)) node
  (dd manager)
  (f node)
  (numVars :int)
  (threshold :int)
  (quality :double))
(defcfun ("Cudd_BiasedUnderApprox" #.(lispify "Cudd_BiasedUnderApprox" :function)) node
  (dd manager)
  (f node)
  (b node)
  (numVars :int)
  (threshold :int)
  (quality1 :double)
  (quality0 :double))
(defcfun ("Cudd_BiasedOverApprox" #.(lispify "Cudd_BiasedOverApprox" :function)) node
  (dd manager)
  (f node)
  (b node)
  (numVars :int)
  (threshold :int)
  (quality1 :double)
  (quality0 :double))
(defcfun ("Cudd_bddExistAbstract" #.(lispify "Cudd_bddExistAbstract" :function)) node
  (manager manager)
  (f node)
  (cube node))
(defcfun ("Cudd_bddXorExistAbstract" #.(lispify "Cudd_bddXorExistAbstract" :function)) node
  (manager manager)
  (f node)
  (g node)
  (cube node))
(defcfun ("Cudd_bddUnivAbstract" #.(lispify "Cudd_bddUnivAbstract" :function)) node
  (manager manager)
  (f node)
  (cube node))
(defcfun ("Cudd_bddBooleanDiff" #.(lispify "Cudd_bddBooleanDiff" :function)) node
  (manager manager)
  (f node)
  (x :int))
(defcfun ("Cudd_bddVarIsDependent" #.(lispify "Cudd_bddVarIsDependent" :function)) :int
  (dd manager)
  (f node)
  (var node))
(defcfun ("Cudd_bddCorrelation" #.(lispify "Cudd_bddCorrelation" :function)) :double
  (manager manager)
  (f node)
  (g node))
(defcfun ("Cudd_bddCorrelationWeights" #.(lispify "Cudd_bddCorrelationWeights" :function)) :double
  (manager manager)
  (f node)
  (g node)
  (prob :pointer))
(defcfun ("Cudd_bddIte" #.(lispify "Cudd_bddIte" :function)) node
  (dd manager)
  (f node)
  (g node)
  (h node))
(defcfun ("Cudd_bddIteConstant" #.(lispify "Cudd_bddIteConstant" :function)) node
  (dd manager)
  (f node)
  (g node)
  (h node))
(defcfun ("Cudd_bddIntersect" #.(lispify "Cudd_bddIntersect" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_bddAnd" #.(lispify "Cudd_bddAnd" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_bddAndLimit" #.(lispify "Cudd_bddAndLimit" :function)) node
  (dd manager)
  (f node)
  (g node)
  (limit :unsigned-int))
(defcfun ("Cudd_bddOr" #.(lispify "Cudd_bddOr" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_bddNand" #.(lispify "Cudd_bddNand" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_bddNor" #.(lispify "Cudd_bddNor" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_bddXor" #.(lispify "Cudd_bddXor" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_bddXnor" #.(lispify "Cudd_bddXnor" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_bddLeq" #.(lispify "Cudd_bddLeq" :function)) :int
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_addBddThreshold" #.(lispify "Cudd_addBddThreshold" :function)) node
  (dd manager)
  (f node)
  (value :double))
(defcfun ("Cudd_addBddStrictThreshold" #.(lispify "Cudd_addBddStrictThreshold" :function)) node
  (dd manager)
  (f node)
  (value :double))
(defcfun ("Cudd_addBddInterval" #.(lispify "Cudd_addBddInterval" :function)) node
  (dd manager)
  (f node)
  (lower :double)
  (upper :double))
(defcfun ("Cudd_addBddIthBit" #.(lispify "Cudd_addBddIthBit" :function)) node
  (dd manager)
  (f node)
  (bit :int))
(defcfun ("Cudd_BddToAdd" #.(lispify "Cudd_BddToAdd" :function)) node
  (dd manager)
  (B node))
(defcfun ("Cudd_addBddPattern" #.(lispify "Cudd_addBddPattern" :function)) node
  (dd manager)
  (f node))
(defcfun ("Cudd_bddTransfer" #.(lispify "Cudd_bddTransfer" :function)) node
  (ddSource manager)
  (ddDestination manager)
  (f node))
(defcfun ("Cudd_DebugCheck" #.(lispify "Cudd_DebugCheck" :function)) :int
  (table manager))
(defcfun ("Cudd_CheckKeys" #.(lispify "Cudd_CheckKeys" :function)) :int
  (table manager))
(defcfun ("Cudd_bddClippingAnd" #.(lispify "Cudd_bddClippingAnd" :function)) node
  (dd manager)
  (f node)
  (g node)
  (maxDepth :int)
  (direction :int))
(defcfun ("Cudd_bddClippingAndAbstract" #.(lispify "Cudd_bddClippingAndAbstract" :function)) node
  (dd manager)
  (f node)
  (g node)
  (cube node)
  (maxDepth :int)
  (direction :int))
(defcfun ("Cudd_Cofactor" #.(lispify "Cudd_Cofactor" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_bddCompose" #.(lispify "Cudd_bddCompose" :function)) node
  (dd manager)
  (f node)
  (g node)
  (v :int))
(defcfun ("Cudd_addCompose" #.(lispify "Cudd_addCompose" :function)) node
  (dd manager)
  (f node)
  (g node)
  (v :int))
(defcfun ("Cudd_addPermute" #.(lispify "Cudd_addPermute" :function)) node
  (manager manager)
  (node node)
  (permut :pointer))
(defcfun ("Cudd_addSwapVariables" #.(lispify "Cudd_addSwapVariables" :function)) node
  (dd manager)
  (f node)
  (x :pointer)
  (y :pointer)
  (n :int))
(defcfun ("Cudd_bddPermute" #.(lispify "Cudd_bddPermute" :function)) node
  (manager manager)
  (node node)
  (permut :pointer))
(defcfun ("Cudd_bddVarMap" #.(lispify "Cudd_bddVarMap" :function)) node
  (manager manager)
  (f node))
(defcfun ("Cudd_SetVarMap" #.(lispify "Cudd_SetVarMap" :function)) :int
  (manager manager)
  (x :pointer)
  (y :pointer)
  (n :int))
(defcfun ("Cudd_bddSwapVariables" #.(lispify "Cudd_bddSwapVariables" :function)) node
  (dd manager)
  (f node)
  (x :pointer)
  (y :pointer)
  (n :int))
(defcfun ("Cudd_bddAdjPermuteX" #.(lispify "Cudd_bddAdjPermuteX" :function)) node
  (dd manager)
  (B node)
  (x :pointer)
  (n :int))
(defcfun ("Cudd_addVectorCompose" #.(lispify "Cudd_addVectorCompose" :function)) node
  (dd manager)
  (f node)
  (vector :pointer))
(defcfun ("Cudd_addGeneralVectorCompose" #.(lispify "Cudd_addGeneralVectorCompose" :function)) node
  (dd manager)
  (f node)
  (vectorOn :pointer)
  (vectorOff :pointer))
(defcfun ("Cudd_addNonSimCompose" #.(lispify "Cudd_addNonSimCompose" :function)) node
  (dd manager)
  (f node)
  (vector :pointer))
(defcfun ("Cudd_bddVectorCompose" #.(lispify "Cudd_bddVectorCompose" :function)) node
  (dd manager)
  (f node)
  (vector :pointer))
(defcfun ("Cudd_bddApproxConjDecomp" #.(lispify "Cudd_bddApproxConjDecomp" :function)) :int
  (dd manager)
  (f node)
  (conjuncts :pointer))
(defcfun ("Cudd_bddApproxDisjDecomp" #.(lispify "Cudd_bddApproxDisjDecomp" :function)) :int
  (dd manager)
  (f node)
  (disjuncts :pointer))
(defcfun ("Cudd_bddIterConjDecomp" #.(lispify "Cudd_bddIterConjDecomp" :function)) :int
  (dd manager)
  (f node)
  (conjuncts :pointer))
(defcfun ("Cudd_bddIterDisjDecomp" #.(lispify "Cudd_bddIterDisjDecomp" :function)) :int
  (dd manager)
  (f node)
  (disjuncts :pointer))
(defcfun ("Cudd_bddGenConjDecomp" #.(lispify "Cudd_bddGenConjDecomp" :function)) :int
  (dd manager)
  (f node)
  (conjuncts :pointer))
(defcfun ("Cudd_bddGenDisjDecomp" #.(lispify "Cudd_bddGenDisjDecomp" :function)) :int
  (dd manager)
  (f node)
  (disjuncts :pointer))
(defcfun ("Cudd_bddVarConjDecomp" #.(lispify "Cudd_bddVarConjDecomp" :function)) :int
  (dd manager)
  (f node)
  (conjuncts :pointer))
(defcfun ("Cudd_bddVarDisjDecomp" #.(lispify "Cudd_bddVarDisjDecomp" :function)) :int
  (dd manager)
  (f node)
  (disjuncts :pointer))
(defcfun ("Cudd_FindEssential" #.(lispify "Cudd_FindEssential" :function)) node
  (dd manager)
  (f node))
(defcfun ("Cudd_bddIsVarEssential" #.(lispify "Cudd_bddIsVarEssential" :function)) :int
  (manager manager)
  (f node)
  (id :int)
  (phase :int))
(defcfun ("Cudd_FindTwoLiteralClauses" #.(lispify "Cudd_FindTwoLiteralClauses" :function)) :pointer
  (dd manager)
  (f node))
(defcfun ("Cudd_PrintTwoLiteralClauses" #.(lispify "Cudd_PrintTwoLiteralClauses" :function)) :int
  (dd manager)
  (f node)
  (names :pointer)
  (fp :pointer))
(defcfun ("Cudd_ReadIthClause" #.(lispify "Cudd_ReadIthClause" :function)) :int
  (tlc :pointer)
  (i :int)
  (var1 :pointer)
  (var2 :pointer)
  (phase1 :pointer)
  (phase2 :pointer))
(defcfun ("Cudd_tlcInfoFree" #.(lispify "Cudd_tlcInfoFree" :function)) :void
  (t_arg0 :pointer))
(defcfun ("Cudd_DumpBlif" #.(lispify "Cudd_DumpBlif" :function)) :int
  (dd manager)
  (n :int)
  (f :pointer)
  (inames :pointer)
  (onames :pointer)
  (mname :string)
  (fp :pointer))
(defcfun ("Cudd_DumpBlifBody" #.(lispify "Cudd_DumpBlifBody" :function)) :int
  (dd manager)
  (n :int)
  (f :pointer)
  (inames :pointer)
  (onames :pointer)
  (fp :pointer))
(defcfun ("Cudd_DumpDot" #.(lispify "Cudd_DumpDot" :function)) :int
  (dd manager)
  (n :int)
  (f :pointer)
  (inames :pointer)
  (onames :pointer)
  (fp :pointer))
(defcfun ("Cudd_DumpDaVinci" #.(lispify "Cudd_DumpDaVinci" :function)) :int
  (dd manager)
  (n :int)
  (f :pointer)
  (inames :pointer)
  (onames :pointer)
  (fp :pointer))
(defcfun ("Cudd_DumpDDcal" #.(lispify "Cudd_DumpDDcal" :function)) :int
  (dd manager)
  (n :int)
  (f :pointer)
  (inames :pointer)
  (onames :pointer)
  (fp :pointer))
(defcfun ("Cudd_DumpFactoredForm" #.(lispify "Cudd_DumpFactoredForm" :function)) :int
  (dd manager)
  (n :int)
  (f :pointer)
  (inames :pointer)
  (onames :pointer)
  (fp :pointer))
(defcfun ("Cudd_bddConstrain" #.(lispify "Cudd_bddConstrain" :function)) node
  (dd manager)
  (f node)
  (c node))
(defcfun ("Cudd_bddRestrict" #.(lispify "Cudd_bddRestrict" :function)) node
  (dd manager)
  (f node)
  (c node))
(defcfun ("Cudd_bddNPAnd" #.(lispify "Cudd_bddNPAnd" :function)) node
  (dd manager)
  (f node)
  (c node))
(defcfun ("Cudd_addConstrain" #.(lispify "Cudd_addConstrain" :function)) node
  (dd manager)
  (f node)
  (c node))
(defcfun ("Cudd_bddConstrainDecomp" #.(lispify "Cudd_bddConstrainDecomp" :function)) :pointer
  (dd manager)
  (f node))
(defcfun ("Cudd_addRestrict" #.(lispify "Cudd_addRestrict" :function)) node
  (dd manager)
  (f node)
  (c node))
(defcfun ("Cudd_bddCharToVect" #.(lispify "Cudd_bddCharToVect" :function)) :pointer
  (dd manager)
  (f node))
(defcfun ("Cudd_bddLICompaction" #.(lispify "Cudd_bddLICompaction" :function)) node
  (dd manager)
  (f node)
  (c node))
(defcfun ("Cudd_bddSqueeze" #.(lispify "Cudd_bddSqueeze" :function)) node
  (dd manager)
  (l node)
  (u node))
(defcfun ("Cudd_bddMinimize" #.(lispify "Cudd_bddMinimize" :function)) node
  (dd manager)
  (f node)
  (c node))
(defcfun ("Cudd_SubsetCompress" #.(lispify "Cudd_SubsetCompress" :function)) node
  (dd manager)
  (f node)
  (nvars :int)
  (threshold :int))
(defcfun ("Cudd_SupersetCompress" #.(lispify "Cudd_SupersetCompress" :function)) node
  (dd manager)
  (f node)
  (nvars :int)
  (threshold :int))
(defcfun ("Cudd_MakeTreeNode" #.(lispify "Cudd_MakeTreeNode" :function)) :pointer
  (dd manager)
  (low :unsigned-int)
  (size :unsigned-int)
  (type :unsigned-int))
(defcfun ("Cudd_addHarwell" #.(lispify "Cudd_addHarwell" :function)) :int
  (fp :pointer)
  (dd manager)
  (E :pointer)
  (x :pointer)
  (y :pointer)
  (xn :pointer)
  (yn_ :pointer)
  (nx :pointer)
  (ny :pointer)
  (m :pointer)
  (n :pointer)
  (bx :int)
  (sx :int)
  (by :int)
  (sy :int)
  (pr :int))
(defcfun ("Cudd_Init" #.(lispify "Cudd_Init" :function)) manager
  (numVars :unsigned-int)
  (numVarsZ :unsigned-int)
  (numSlots :unsigned-int)
  (cacheSize :unsigned-int)
  (maxMemory :unsigned-long))
(defcfun ("Cudd_Quit" #.(lispify "Cudd_Quit" :function)) :void
  (unique manager))
(defcfun ("Cudd_PrintLinear" #.(lispify "Cudd_PrintLinear" :function)) :int
  (table manager))
(defcfun ("Cudd_ReadLinear" #.(lispify "Cudd_ReadLinear" :function)) :int
  (table manager)
  (x :int)
  (y :int))
(defcfun ("Cudd_bddLiteralSetIntersection" #.(lispify "Cudd_bddLiteralSetIntersection" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_addMatrixMultiply" #.(lispify "Cudd_addMatrixMultiply" :function)) node
  (dd manager)
  (A node)
  (B node)
  (z :pointer)
  (nz :int))
(defcfun ("Cudd_addTimesPlus" #.(lispify "Cudd_addTimesPlus" :function)) node
  (dd manager)
  (A node)
  (B node)
  (z :pointer)
  (nz :int))
(defcfun ("Cudd_addTriangle" #.(lispify "Cudd_addTriangle" :function)) node
  (dd manager)
  (f node)
  (g node)
  (z :pointer)
  (nz :int))
(defcfun ("Cudd_addOuterSum" #.(lispify "Cudd_addOuterSum" :function)) node
  (dd manager)
  (M node)
  (r node)
  (c node))
(defcfun ("Cudd_PrioritySelect" #.(lispify "Cudd_PrioritySelect" :function)) node
  (dd manager)
  (R node)
  (x :pointer)
  (y :pointer)
  (z :pointer)
  (Pi node)
  (n :int)
  (arg7 :pointer))
(defcfun ("Cudd_Xgty" #.(lispify "Cudd_Xgty" :function)) node
  (dd manager)
  (N :int)
  (z :pointer)
  (x :pointer)
  (y :pointer))
(defcfun ("Cudd_Xeqy" #.(lispify "Cudd_Xeqy" :function)) node
  (dd manager)
  (N :int)
  (x :pointer)
  (y :pointer))
(defcfun ("Cudd_addXeqy" #.(lispify "Cudd_addXeqy" :function)) node
  (dd manager)
  (N :int)
  (x :pointer)
  (y :pointer))
(defcfun ("Cudd_Dxygtdxz" #.(lispify "Cudd_Dxygtdxz" :function)) node
  (dd manager)
  (N :int)
  (x :pointer)
  (y :pointer)
  (z :pointer))
(defcfun ("Cudd_Dxygtdyz" #.(lispify "Cudd_Dxygtdyz" :function)) node
  (dd manager)
  (N :int)
  (x :pointer)
  (y :pointer)
  (z :pointer))
(defcfun ("Cudd_CProjection" #.(lispify "Cudd_CProjection" :function)) node
  (dd manager)
  (R node)
  (Y node))
(defcfun ("Cudd_addHamming" #.(lispify "Cudd_addHamming" :function)) node
  (dd manager)
  (xVars :pointer)
  (yVars :pointer)
  (nVars :int))
(defcfun ("Cudd_MinHammingDist" #.(lispify "Cudd_MinHammingDist" :function)) :int
  (dd manager)
  (f node)
  (minterm :pointer)
  (upperBound :int))
(defcfun ("Cudd_bddClosestCube" #.(lispify "Cudd_bddClosestCube" :function)) node
  (dd manager)
  (f node)
  (g node)
  (distance :pointer))
(defcfun ("Cudd_addRead" #.(lispify "Cudd_addRead" :function)) :int
  (fp :pointer)
  (dd manager)
  (E :pointer)
  (x :pointer)
  (y :pointer)
  (xn :pointer)
  (yn_ :pointer)
  (nx :pointer)
  (ny :pointer)
  (m :pointer)
  (n :pointer)
  (bx :int)
  (sx :int)
  (by :int)
  (sy :int))
(defcfun ("Cudd_bddRead" #.(lispify "Cudd_bddRead" :function)) :int
  (fp :pointer)
  (dd manager)
  (E :pointer)
  (x :pointer)
  (y :pointer)
  (nx :pointer)
  (ny :pointer)
  (m :pointer)
  (n :pointer)
  (bx :int)
  (sx :int)
  (by :int)
  (sy :int))
(defcfun ("Cudd_Ref" #.(lispify "Cudd_Ref" :function)) :void
  (n node))
(defcfun ("Cudd_RecursiveDeref" #.(lispify "Cudd_RecursiveDeref" :function)) :void
  (table manager)
  (n node))
(defcfun ("Cudd_IterDerefBdd" #.(lispify "Cudd_IterDerefBdd" :function)) :void
  (table manager)
  (n node))
(defcfun ("Cudd_DelayedDerefBdd" #.(lispify "Cudd_DelayedDerefBdd" :function)) :void
  (table manager)
  (n node))
(defcfun ("Cudd_RecursiveDerefZdd" #.(lispify "Cudd_RecursiveDerefZdd" :function)) :void
  (table manager)
  (n node))
(defcfun ("Cudd_Deref" #.(lispify "Cudd_Deref" :function)) :void
  (node node))
(defcfun ("Cudd_CheckZeroRef" #.(lispify "Cudd_CheckZeroRef" :function)) :int
  (manager manager))
(defcfun ("Cudd_ReduceHeap" #.(lispify "Cudd_ReduceHeap" :function)) :int
  (table manager)
  (heuristic #.(lispify "Cudd_ReorderingType" :enumname))
  (minsize :int))
(defcfun ("Cudd_ShuffleHeap" #.(lispify "Cudd_ShuffleHeap" :function)) :int
  (table manager)
  (permutation :pointer))
(defcfun ("Cudd_Eval" #.(lispify "Cudd_Eval" :function)) node
  (dd manager)
  (f node)
  (inputs :pointer))
(defcfun ("Cudd_ShortestPath" #.(lispify "Cudd_ShortestPath" :function)) node
  (manager manager)
  (f node)
  (weight :pointer)
  (support :pointer)
  (length :pointer))
(defcfun ("Cudd_LargestCube" #.(lispify "Cudd_LargestCube" :function)) node
  (manager manager)
  (f node)
  (length :pointer))
(defcfun ("Cudd_ShortestLength" #.(lispify "Cudd_ShortestLength" :function)) :int
  (manager manager)
  (f node)
  (weight :pointer))
(defcfun ("Cudd_Decreasing" #.(lispify "Cudd_Decreasing" :function)) node
  (dd manager)
  (f node)
  (i :int))
(defcfun ("Cudd_Increasing" #.(lispify "Cudd_Increasing" :function)) node
  (dd manager)
  (f node)
  (i :int))
(defcfun ("Cudd_EquivDC" #.(lispify "Cudd_EquivDC" :function)) :int
  (dd manager)
  (F node)
  (G node)
  (D node))
(defcfun ("Cudd_bddLeqUnless" #.(lispify "Cudd_bddLeqUnless" :function)) :int
  (dd manager)
  (f node)
  (g node)
  (D node))
(defcfun ("Cudd_EqualSupNorm" #.(lispify "Cudd_EqualSupNorm" :function)) :int
  (dd manager)
  (f node)
  (g node)
  (tolerance :double)
  (pr :int))
(defcfun ("Cudd_bddMakePrime" #.(lispify "Cudd_bddMakePrime" :function)) node
  (dd manager)
  (cube node)
  (f node))
(defcfun ("Cudd_CofMinterm" #.(lispify "Cudd_CofMinterm" :function)) :pointer
  (dd manager)
  (node node))
(defcfun ("Cudd_SolveEqn" #.(lispify "Cudd_SolveEqn" :function)) node
  (bdd manager)
  (F node)
  (Y node)
  (G :pointer)
  (yIndex :pointer)
  (n :int))
(defcfun ("Cudd_VerifySol" #.(lispify "Cudd_VerifySol" :function)) node
  (bdd manager)
  (F node)
  (G :pointer)
  (yIndex :pointer)
  (n :int))
(defcfun ("Cudd_SplitSet" #.(lispify "Cudd_SplitSet" :function)) node
  (manager manager)
  (S node)
  (xVars :pointer)
  (n :int)
  (m :double))
(defcfun ("Cudd_SubsetHeavyBranch" #.(lispify "Cudd_SubsetHeavyBranch" :function)) node
  (dd manager)
  (f node)
  (numVars :int)
  (threshold :int))
(defcfun ("Cudd_SupersetHeavyBranch" #.(lispify "Cudd_SupersetHeavyBranch" :function)) node
  (dd manager)
  (f node)
  (numVars :int)
  (threshold :int))
(defcfun ("Cudd_SubsetShortPaths" #.(lispify "Cudd_SubsetShortPaths" :function)) node
  (dd manager)
  (f node)
  (numVars :int)
  (threshold :int)
  (hardlimit :int))
(defcfun ("Cudd_SupersetShortPaths" #.(lispify "Cudd_SupersetShortPaths" :function)) node
  (dd manager)
  (f node)
  (numVars :int)
  (threshold :int)
  (hardlimit :int))
(defcfun ("Cudd_SymmProfile" #.(lispify "Cudd_SymmProfile" :function)) :void
  (table manager)
  (lower :int)
  (upper :int))
(defcfun ("Cudd_Prime" #.(lispify "Cudd_Prime" :function)) :unsigned-int
  (p :unsigned-int))
(defcfun ("Cudd_PrintMinterm" #.(lispify "Cudd_PrintMinterm" :function)) :int
  (manager manager)
  (node node))
(defcfun ("Cudd_bddPrintCover" #.(lispify "Cudd_bddPrintCover" :function)) :int
  (dd manager)
  (l node)
  (u node))
(defcfun ("Cudd_PrintDebug" #.(lispify "Cudd_PrintDebug" :function)) :int
  (dd manager)
  (f node)
  (n :int)
  (pr :int))
(defcfun ("Cudd_DagSize" #.(lispify "Cudd_DagSize" :function)) :int
  (node node))
(defcfun ("Cudd_EstimateCofactor" #.(lispify "Cudd_EstimateCofactor" :function)) :int
  (dd manager)
  (node node)
  (i :int)
  (phase :int))
(defcfun ("Cudd_EstimateCofactorSimple" #.(lispify "Cudd_EstimateCofactorSimple" :function)) :int
  (node node)
  (i :int))
(defcfun ("Cudd_SharingSize" #.(lispify "Cudd_SharingSize" :function)) :int
  (nodeArray :pointer)
  (n :int))
(defcfun ("Cudd_CountMinterm" #.(lispify "Cudd_CountMinterm" :function)) :double
  (manager manager)
  (node node)
  (nvars :int))
(defcfun ("Cudd_EpdCountMinterm" #.(lispify "Cudd_EpdCountMinterm" :function)) :int
  (manager manager)
  (node node)
  (nvars :int)
  (epd :pointer))
(defcfun ("Cudd_CountPath" #.(lispify "Cudd_CountPath" :function)) :double
  (node node))
(defcfun ("Cudd_CountPathsToNonZero" #.(lispify "Cudd_CountPathsToNonZero" :function)) :double
  (node node))
(defcfun ("Cudd_Support" #.(lispify "Cudd_Support" :function)) node
  (dd manager)
  (f node))
(defcfun ("Cudd_SupportIndex" #.(lispify "Cudd_SupportIndex" :function)) :pointer
  (dd manager)
  (f node))
(defcfun ("Cudd_SupportSize" #.(lispify "Cudd_SupportSize" :function)) :int
  (dd manager)
  (f node))
(defcfun ("Cudd_VectorSupport" #.(lispify "Cudd_VectorSupport" :function)) node
  (dd manager)
  (F :pointer)
  (n :int))
(defcfun ("Cudd_VectorSupportIndex" #.(lispify "Cudd_VectorSupportIndex" :function)) :pointer
  (dd manager)
  (F :pointer)
  (n :int))
(defcfun ("Cudd_VectorSupportSize" #.(lispify "Cudd_VectorSupportSize" :function)) :int
  (dd manager)
  (F :pointer)
  (n :int))
(defcfun ("Cudd_ClassifySupport" #.(lispify "Cudd_ClassifySupport" :function)) :int
  (dd manager)
  (f node)
  (g node)
  (common :pointer)
  (onlyF :pointer)
  (onlyG :pointer))
(defcfun ("Cudd_CountLeaves" #.(lispify "Cudd_CountLeaves" :function)) :int
  (node node))
(defcfun ("Cudd_bddPickOneCube" #.(lispify "Cudd_bddPickOneCube" :function)) :int
  (ddm manager)
  (node node)
  (string :string))
(defcfun ("Cudd_bddPickOneMinterm" #.(lispify "Cudd_bddPickOneMinterm" :function)) node
  (dd manager)
  (f node)
  (vars :pointer)
  (n :int))
(defcfun ("Cudd_bddPickArbitraryMinterms" #.(lispify "Cudd_bddPickArbitraryMinterms" :function)) :pointer
  (dd manager)
  (f node)
  (vars :pointer)
  (n :int)
  (k :int))
(defcfun ("Cudd_SubsetWithMaskVars" #.(lispify "Cudd_SubsetWithMaskVars" :function)) node
  (dd manager)
  (f node)
  (vars :pointer)
  (nvars :int)
  (maskVars :pointer)
  (mvars :int))
(defcfun ("Cudd_FirstCube" #.(lispify "Cudd_FirstCube" :function)) :pointer
  (dd manager)
  (f node)
  (cube :pointer)
  (value :pointer))
(defcfun ("Cudd_NextCube" #.(lispify "Cudd_NextCube" :function)) :int
  (gen :pointer)
  (cube :pointer)
  (value :pointer))
(defcfun ("Cudd_FirstPrime" #.(lispify "Cudd_FirstPrime" :function)) :pointer
  (dd manager)
  (l node)
  (u node)
  (cube :pointer))
(defcfun ("Cudd_NextPrime" #.(lispify "Cudd_NextPrime" :function)) :int
  (gen :pointer)
  (cube :pointer))
(defcfun ("Cudd_bddComputeCube" #.(lispify "Cudd_bddComputeCube" :function)) node
  (dd manager)
  (vars :pointer)
  (phase :pointer)
  (n :int))
(defcfun ("Cudd_addComputeCube" #.(lispify "Cudd_addComputeCube" :function)) node
  (dd manager)
  (vars :pointer)
  (phase :pointer)
  (n :int))
(defcfun ("Cudd_CubeArrayToBdd" #.(lispify "Cudd_CubeArrayToBdd" :function)) node
  (dd manager)
  (array :pointer))
(defcfun ("Cudd_BddToCubeArray" #.(lispify "Cudd_BddToCubeArray" :function)) :int
  (dd manager)
  (cube node)
  (array :pointer))
(defcfun ("Cudd_FirstNode" #.(lispify "Cudd_FirstNode" :function)) :pointer
  (dd manager)
  (f node)
  (node :pointer))
(defcfun ("Cudd_NextNode" #.(lispify "Cudd_NextNode" :function)) :int
  (gen :pointer)
  (node :pointer))
(defcfun ("Cudd_GenFree" #.(lispify "Cudd_GenFree" :function)) :int
  (gen :pointer))
(defcfun ("Cudd_IsGenEmpty" #.(lispify "Cudd_IsGenEmpty" :function)) :int
  (gen :pointer))
(defcfun ("Cudd_IndicesToCube" #.(lispify "Cudd_IndicesToCube" :function)) node
  (dd manager)
  (array :pointer)
  (n :int))
(defcfun ("Cudd_PrintVersion" #.(lispify "Cudd_PrintVersion" :function)) :void
  (fp :pointer))
(defcfun ("Cudd_AverageDistance" #.(lispify "Cudd_AverageDistance" :function)) :double
  (dd manager))
(defcfun ("Cudd_Random" #.(lispify "Cudd_Random" :function)) :long)
(defcfun ("Cudd_Srandom" #.(lispify "Cudd_Srandom" :function)) :void
  (seed :long))
(defcfun ("Cudd_Density" #.(lispify "Cudd_Density" :function)) :double
  (dd manager)
  (f node)
  (nvars :int))
(defcfun ("Cudd_OutOfMem" #.(lispify "Cudd_OutOfMem" :function)) :void
  (size :long))
(defcfun ("Cudd_zddCount" #.(lispify "Cudd_zddCount" :function)) :int
  (zdd manager)
  (P node))
(defcfun ("Cudd_zddCountDouble" #.(lispify "Cudd_zddCountDouble" :function)) :double
  (zdd manager)
  (P node))
(defcfun ("Cudd_zddProduct" #.(lispify "Cudd_zddProduct" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_zddUnateProduct" #.(lispify "Cudd_zddUnateProduct" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_zddWeakDiv" #.(lispify "Cudd_zddWeakDiv" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_zddDivide" #.(lispify "Cudd_zddDivide" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_zddWeakDivF" #.(lispify "Cudd_zddWeakDivF" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_zddDivideF" #.(lispify "Cudd_zddDivideF" :function)) node
  (dd manager)
  (f node)
  (g node))
(defcfun ("Cudd_zddComplement" #.(lispify "Cudd_zddComplement" :function)) node
  (dd manager)
  (node node))
(defcfun ("Cudd_MakeZddTreeNode" #.(lispify "Cudd_MakeZddTreeNode" :function)) :pointer
  (dd manager)
  (low :unsigned-int)
  (size :unsigned-int)
  (type :unsigned-int))
(defcfun ("Cudd_zddIsop" #.(lispify "Cudd_zddIsop" :function)) node
  (dd manager)
  (L node)
  (U node)
  (zdd_I :pointer))
(defcfun ("Cudd_bddIsop" #.(lispify "Cudd_bddIsop" :function)) node
  (dd manager)
  (L node)
  (U node))
(defcfun ("Cudd_MakeBddFromZddCover" #.(lispify "Cudd_MakeBddFromZddCover" :function)) node
  (dd manager)
  (node node))
(defcfun ("Cudd_zddDagSize" #.(lispify "Cudd_zddDagSize" :function)) :int
  (p_node node))
(defcfun ("Cudd_zddCountMinterm" #.(lispify "Cudd_zddCountMinterm" :function)) :double
  (zdd manager)
  (node node)
  (path :int))
(defcfun ("Cudd_zddPrintSubtable" #.(lispify "Cudd_zddPrintSubtable" :function)) :void
  (table manager))
(defcfun ("Cudd_zddPortFromBdd" #.(lispify "Cudd_zddPortFromBdd" :function)) node
  (dd manager)
  (B node))
(defcfun ("Cudd_zddPortToBdd" #.(lispify "Cudd_zddPortToBdd" :function)) node
  (dd manager)
  (f node))
(defcfun ("Cudd_zddReduceHeap" #.(lispify "Cudd_zddReduceHeap" :function)) :int
  (table manager)
  (heuristic #.(lispify "Cudd_ReorderingType" :enumname))
  (minsize :int))
(defcfun ("Cudd_zddShuffleHeap" #.(lispify "Cudd_zddShuffleHeap" :function)) :int
  (table manager)
  (permutation :pointer))
(defcfun ("Cudd_zddIte" #.(lispify "Cudd_zddIte" :function)) node
  (dd manager)
  (f node)
  (g node)
  (h node))
(defcfun ("Cudd_zddUnion" #.(lispify "Cudd_zddUnion" :function)) node
  (dd manager)
  (P node)
  (Q node))
(defcfun ("Cudd_zddIntersect" #.(lispify "Cudd_zddIntersect" :function)) node
  (dd manager)
  (P node)
  (Q node))
(defcfun ("Cudd_zddDiff" #.(lispify "Cudd_zddDiff" :function)) node
  (dd manager)
  (P node)
  (Q node))
(defcfun ("Cudd_zddDiffConst" #.(lispify "Cudd_zddDiffConst" :function)) node
  (zdd manager)
  (P node)
  (Q node))
(defcfun ("Cudd_zddSubset1" #.(lispify "Cudd_zddSubset1" :function)) node
  (dd manager)
  (P node)
  (var :int))
(defcfun ("Cudd_zddSubset0" #.(lispify "Cudd_zddSubset0" :function)) node
  (dd manager)
  (P node)
  (var :int))
(defcfun ("Cudd_zddChange" #.(lispify "Cudd_zddChange" :function)) node
  (dd manager)
  (P node)
  (var :int))
(defcfun ("Cudd_zddSymmProfile" #.(lispify "Cudd_zddSymmProfile" :function)) :void
  (table manager)
  (lower :int)
  (upper :int))
(defcfun ("Cudd_zddPrintMinterm" #.(lispify "Cudd_zddPrintMinterm" :function)) :int
  (zdd manager)
  (node node))
(defcfun ("Cudd_zddPrintCover" #.(lispify "Cudd_zddPrintCover" :function)) :int
  (zdd manager)
  (node node))
(defcfun ("Cudd_zddPrintDebug" #.(lispify "Cudd_zddPrintDebug" :function)) :int
  (zdd manager)
  (f node)
  (n :int)
  (pr :int))
(defcfun ("Cudd_zddFirstPath" #.(lispify "Cudd_zddFirstPath" :function)) :pointer
  (zdd manager)
  (f node)
  (path :pointer))
(defcfun ("Cudd_zddNextPath" #.(lispify "Cudd_zddNextPath" :function)) :int
  (gen :pointer)
  (path :pointer))
(defcfun ("Cudd_zddCoverPathToString" #.(lispify "Cudd_zddCoverPathToString" :function)) :string
  (zdd manager)
  (path :pointer)
  (str :string))
(defcfun ("Cudd_zddDumpDot" #.(lispify "Cudd_zddDumpDot" :function)) :int
  (dd manager)
  (n :int)
  (f :pointer)
  (inames :pointer)
  (onames :pointer)
  (fp :pointer))
(defcfun ("Cudd_bddSetPiVar" #.(lispify "Cudd_bddSetPiVar" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddSetPsVar" #.(lispify "Cudd_bddSetPsVar" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddSetNsVar" #.(lispify "Cudd_bddSetNsVar" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddIsPiVar" #.(lispify "Cudd_bddIsPiVar" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddIsPsVar" #.(lispify "Cudd_bddIsPsVar" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddIsNsVar" #.(lispify "Cudd_bddIsNsVar" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddSetPairIndex" #.(lispify "Cudd_bddSetPairIndex" :function)) :int
  (dd manager)
  (index :int)
  (pairIndex :int))
(defcfun ("Cudd_bddReadPairIndex" #.(lispify "Cudd_bddReadPairIndex" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddSetVarToBeGrouped" #.(lispify "Cudd_bddSetVarToBeGrouped" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddSetVarHardGroup" #.(lispify "Cudd_bddSetVarHardGroup" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddResetVarToBeGrouped" #.(lispify "Cudd_bddResetVarToBeGrouped" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddIsVarToBeGrouped" #.(lispify "Cudd_bddIsVarToBeGrouped" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddSetVarToBeUngrouped" #.(lispify "Cudd_bddSetVarToBeUngrouped" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddIsVarToBeUngrouped" #.(lispify "Cudd_bddIsVarToBeUngrouped" :function)) :int
  (dd manager)
  (index :int))
(defcfun ("Cudd_bddIsVarHardGroup" #.(lispify "Cudd_bddIsVarHardGroup" :function)) :int
  (dd manager)
  (index :int))
