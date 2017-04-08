module Equalox.Implications 
where

implications =   [
        ("transitive",["total","symmetric"]),
  
        ("transitive",["symmetric","total_neg"]),
  
        ("transitive",["antisymmetric","coreflexive_neg"]),
  
        ("transitive",["coreflexive"]),
  
        ("transitive",["transitive_neg","right_euclidean"]),
  
        ("transitive",["reflexive","right_euclidean_neg"]),
  
        ("transitive",["reflexive_neg","right_euclidean"]),
  
        ("transitive",["antisymmetric","transitive_neg"]),
  
        ("transitive",["total","right_euclidean_neg"]),
  
        ("transitive",["reflexive","coreflexive_neg"]),
  
        ("transitive",["total_neg","coreflexive_neg"]),
  
        ("transitive",["total_neg","right_euclidean"]),
  
        ("transitive",["symmetric","antisymmetric"]),
  
        ("transitive",["symmetric","right_euclidean"]),
  
        ("transitive",["reflexive","symmetric","antisymmetric_neg"]),
  
        ("transitive",["right_euclidean","right_euclidean_neg"]),
  
        ("transitive",["total","right_euclidean"]),
  
        ("transitive",["total","coreflexive_neg"]),
  
        ("transitive",["total_neg","right_euclidean_neg"]),
  
        ("transitive",["total_neg","transitive_neg"]),
  
        ("transitive",["reflexive","right_euclidean_neg_flip"]),
  
        ("transitive",["symmetric","right_euclidean_flip"]),
  
        ("transitive",["reflexive","right_euclidean"]),
  
        ("transitive",["antisymmetric","right_euclidean_neg"]),
  
        ("transitive",["antisymmetric","right_euclidean"]),
  
        ("transitive",["total_neg","right_euclidean_flip"]),
  
        ("transitive",["coreflexive_neg","right_euclidean_flip"]),
  
        ("transitive",["reflexive","right_euclidean_flip"]),
  
        ("transitive",["total","right_euclidean_flip"]),
  
        ("transitive",["serial_flip","right_euclidean"]),
  
        ("transitive",["total","right_euclidean_neg_flip"]),
  
        ("transitive",["total_neg","right_euclidean_neg_flip"]),
  
        ("transitive",["coreflexive_neg","right_euclidean"]),
  
        ("transitive",["right_euclidean","antisymmetric_neg"]),
  
        ("transitive",["right_euclidean","right_euclidean_flip"]),
  
        ("transitive",["reflexive","symmetric","transitive_neg"]),
  
        ("transitive",["antisymmetric","right_euclidean_neg_flip"]),
  
        ("transitive",["transitive_neg","right_euclidean_flip"]),
  
        ("transitive",["antisymmetric","right_euclidean_flip"]),
  
        ("transitive",["serial","right_euclidean_flip"]),
  
        ("transitive",["antisymmetric_neg","right_euclidean_flip"]),
  
        ("transitive",["right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("transitive",["reflexive_neg","right_euclidean_flip"]),
  
        ("transitive",["right_euclidean","right_euclidean_neg_flip"]),
  
        ("transitive",["right_euclidean_neg","right_euclidean_flip"]),
  
        ("reflexive",["serial","antisymmetric","coreflexive_neg"]),
  
        ("reflexive",["antisymmetric","serial_flip","coreflexive_neg"]),
  
        ("reflexive",["total"]),
  
        ("reflexive",["transitive","serial_flip","right_euclidean_neg"]),
  
        ("reflexive",["serial","coreflexive"]),
  
        ("reflexive",["serial_flip","right_euclidean"]),
  
        ("reflexive",["symmetric","transitive","serial_flip"]),
  
        ("reflexive",["symmetric","serial_flip","right_euclidean_flip"]),
  
        ("reflexive",["serial","symmetric","right_euclidean"]),
  
        ("reflexive",["serial","right_euclidean_flip"]),
  
        ("reflexive",["serial","symmetric","transitive"]),
  
        ("reflexive",["transitive","serial_flip","coreflexive_neg"]),
  
        ("reflexive",["coreflexive","serial_flip"]),
  
        ("reflexive",["serial","symmetric","antisymmetric"]),
  
        ("reflexive",["serial","transitive","right_euclidean_neg_flip"]),
  
        ("reflexive",["serial","antisymmetric","right_euclidean_neg_flip"]),
  
        ("reflexive",["symmetric","antisymmetric","serial_flip"]),
  
        ("reflexive",["serial_flip","right_euclidean_neg","right_euclidean_flip"]),
  
        ("reflexive",["antisymmetric","serial_flip","right_euclidean_neg"]),
  
        ("reflexive",["serial","transitive","coreflexive_neg"]),
  
        ("reflexive",["serial","right_euclidean","right_euclidean_neg_flip"]),
  
        ("reflexive",["serial","coreflexive_neg","right_euclidean"]),
  
        ("reflexive",["serial_flip","coreflexive_neg","right_euclidean_flip"]),
  
        ("symmetric",["transitive","serial_flip","right_euclidean_neg"]),
  
        ("symmetric",["coreflexive"]),
  
        ("symmetric",["reflexive_neg","right_euclidean"]),
  
        ("symmetric",["reflexive_neg","right_euclidean_flip"]),
  
        ("symmetric",["coreflexive_neg"]),
  
        ("symmetric",["reflexive","right_euclidean_neg"]),
  
        ("symmetric",["reflexive","right_euclidean_neg_flip"]),
  
        ("symmetric",["reflexive","right_euclidean"]),
  
        ("symmetric",["total_neg","right_euclidean"]),
  
        ("symmetric",["transitive_neg","right_euclidean","serial_neg_flip"]),
  
        ("symmetric",["reflexive_neg","right_euclidean_neg"]),
  
        ("symmetric",["total_neg","right_euclidean_neg"]),
  
        ("symmetric",["total","right_euclidean_neg"]),
  
        ("symmetric",["total","right_euclidean"]),
  
        ("symmetric",["total","right_euclidean_flip"]),
  
        ("symmetric",["total","right_euclidean_neg_flip"]),
  
        ("symmetric",["serial_flip","right_euclidean"]),
  
        ("symmetric",["total_neg","right_euclidean_flip"]),
  
        ("symmetric",["reflexive_neg","right_euclidean_neg_flip"]),
  
        ("symmetric",["right_euclidean","right_euclidean_neg_flip"]),
  
        ("symmetric",["reflexive","right_euclidean_flip"]),
  
        ("symmetric",["right_euclidean_neg","right_euclidean_flip"]),
  
        ("symmetric",["right_euclidean","right_euclidean_flip"]),
  
        ("symmetric",["serial_neg","transitive_neg","right_euclidean_flip"]),
  
        ("symmetric",["serial_neg","right_euclidean_neg_flip"]),
  
        ("symmetric",["right_euclidean_neg","right_euclidean_neg_flip"]),
  
        ("symmetric",["serial","transitive","right_euclidean_neg_flip"]),
  
        ("symmetric",["antisymmetric","serial_flip","right_euclidean_neg"]),
  
        ("symmetric",["serial_neg_flip","right_euclidean_neg"]),
  
        ("symmetric",["serial","right_euclidean_flip"]),
  
        ("symmetric",["serial","antisymmetric","right_euclidean_neg_flip"]),
  
        ("symmetric",["total_neg","right_euclidean_neg_flip"]),
  
        ("symmetric",["right_euclidean","antisymmetric_neg","serial_neg_flip"]),
  
        ("symmetric",["serial_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("right_euclidean",["symmetric","total_neg"]),
  
        ("right_euclidean",["total","symmetric"]),
  
        ("right_euclidean",["antisymmetric","coreflexive_neg"]),
  
        ("right_euclidean",["transitive","right_euclidean_neg"]),
  
        ("right_euclidean",["reflexive","right_euclidean_neg"]),
  
        ("right_euclidean",["reflexive","coreflexive_neg"]),
  
        ("right_euclidean",["total_neg","right_euclidean_neg"]),
  
        ("right_euclidean",["coreflexive"]),
  
        ("right_euclidean",["reflexive_neg","right_euclidean_flip"]),
  
        ("right_euclidean",["total","coreflexive_neg"]),
  
        ("right_euclidean",["total","right_euclidean_neg"]),
  
        ("right_euclidean",["transitive","reflexive_neg","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["reflexive","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["transitive","coreflexive_neg"]),
  
        ("right_euclidean",["total_neg","right_euclidean_flip"]),
  
        ("right_euclidean",["serial_neg","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["symmetric","antisymmetric"]),
  
        ("right_euclidean",["total","right_euclidean_flip"]),
  
        ("right_euclidean",["serial_neg","transitive_neg","right_euclidean_flip"]),
  
        ("right_euclidean",["transitive","serial_neg","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["total_neg","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["antisymmetric","right_euclidean_neg"]),
  
        ("right_euclidean",["symmetric","transitive"]),
  
        ("right_euclidean",["total","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["reflexive","symmetric","antisymmetric_neg"]),
  
        ("right_euclidean",["serial","transitive","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["reflexive","right_euclidean_flip"]),
  
        ("right_euclidean",["right_euclidean_neg","right_euclidean_flip"]),
  
        ("right_euclidean",["total_neg","coreflexive_neg"]),
  
        ("right_euclidean",["antisymmetric","reflexive_neg","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["antisymmetric","serial_neg","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["serial","antisymmetric","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["reflexive","symmetric","transitive_neg"]),
  
        ("right_euclidean",["serial","right_euclidean_flip"]),
  
        ("right_euclidean",["symmetric","right_euclidean_flip"]),
  
        ("right_euclidean",["serial_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("right_euclidean",["coreflexive_neg","right_euclidean_flip"]),
  
        ("coreflexive",["antisymmetric","coreflexive_neg"]),
  
        ("coreflexive",["symmetric","total_neg"]),
  
        ("coreflexive",["transitive","reflexive_neg","right_euclidean_neg"]),
  
        ("coreflexive",["reflexive","antisymmetric","right_euclidean_neg"]),
  
        ("coreflexive",["symmetric","antisymmetric"]),
  
        ("coreflexive",["reflexive_neg","right_euclidean"]),
  
        ("coreflexive",["reflexive_neg","right_euclidean_flip"]),
  
        ("coreflexive",["total","antisymmetric","right_euclidean"]),
  
        ("coreflexive",["reflexive","antisymmetric","right_euclidean"]),
  
        ("coreflexive",["reflexive","antisymmetric","right_euclidean_flip"]),
  
        ("coreflexive",["transitive_neg","right_euclidean","serial_neg_flip"]),
  
        ("coreflexive",["total_neg","right_euclidean"]),
  
        ("coreflexive",["total","antisymmetric","right_euclidean_neg"]),
  
        ("coreflexive",["total","antisymmetric","right_euclidean_flip"]),
  
        ("coreflexive",["reflexive","antisymmetric","right_euclidean_neg_flip"]),
  
        ("coreflexive",["serial_neg","coreflexive_neg","right_euclidean"]),
  
        ("coreflexive",["serial_neg","coreflexive_neg","right_euclidean_flip"]),
  
        ("coreflexive",["coreflexive_neg","right_euclidean","serial_neg_flip"]),
  
        ("coreflexive",["transitive","serial_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive",["coreflexive_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive",["total_neg","coreflexive_neg"]),
  
        ("coreflexive",["antisymmetric","right_euclidean","right_euclidean_neg_flip"]),
  
        ("coreflexive",["antisymmetric","serial_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive",["antisymmetric","serial_flip","right_euclidean"]),
  
        ("coreflexive",["total_neg","right_euclidean_flip"]),
  
        ("coreflexive",["total","antisymmetric","right_euclidean_neg_flip"]),
  
        ("coreflexive",["antisymmetric","right_euclidean_neg","right_euclidean_flip"]),
  
        ("coreflexive",["serial_neg","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive",["symmetric","transitive","serial_neg_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive",["symmetric","antisymmetric_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive",["transitive","serial_neg","coreflexive_neg"]),
  
        ("coreflexive",["serial_neg","transitive_neg","right_euclidean_flip"]),
  
        ("coreflexive",["antisymmetric","reflexive_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive",["symmetric","transitive","reflexive_neg"]),
  
        ("coreflexive",["antisymmetric","right_euclidean","right_euclidean_flip"]),
  
        ("coreflexive",["symmetric","transitive","serial_neg","right_euclidean_neg"]),
  
        ("coreflexive",["transitive","reflexive_neg","coreflexive_neg"]),
  
        ("coreflexive",["transitive","reflexive_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive",["antisymmetric","reflexive_neg","right_euclidean_neg"]),
  
        ("coreflexive",["total_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive",["total_neg","right_euclidean_neg"]),
  
        ("coreflexive",["antisymmetric","right_euclidean_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive",["serial","antisymmetric","right_euclidean_neg_flip"]),
  
        ("coreflexive",["serial","antisymmetric","right_euclidean_flip"]),
  
        ("coreflexive",["serial_neg_flip","right_euclidean_neg","right_euclidean_flip"]),
  
        ("coreflexive",["transitive","coreflexive_neg","serial_neg_flip"]),
  
        ("coreflexive",["symmetric","transitive","serial_neg","antisymmetric_neg"]),
  
        ("coreflexive",["serial_neg","right_euclidean","right_euclidean_neg_flip"]),
  
        ("coreflexive",["right_euclidean","serial_neg_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive",["right_euclidean","antisymmetric_neg","serial_neg_flip"]),
  
        ("coreflexive",["symmetric","serial_neg","transitive_neg","right_euclidean"]),
  
        ("coreflexive",["symmetric","serial_neg","right_euclidean","antisymmetric_neg"]),
  
        ("coreflexive",["symmetric","serial_neg","right_euclidean","right_euclidean_neg"]),
  
        ("coreflexive",["transitive","serial_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive",["symmetric","transitive","serial_neg","transitive_neg"]),
  
        ("coreflexive",["symmetric","transitive","antisymmetric_neg","serial_neg_flip"]),
  
        ("coreflexive",["symmetric","serial_neg_flip","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive",["symmetric","transitive","transitive_neg","serial_neg_flip"]),
  
        ("coreflexive",["antisymmetric","serial_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive",["antisymmetric","serial_flip","right_euclidean_neg"]),
  
        ("coreflexive",["symmetric","transitive_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive",["serial_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("coreflexive",["right_euclidean","serial_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive",["serial_neg","right_euclidean_neg","right_euclidean_flip"]),
  
        ("antisymmetric",["total_neg"]),
  
        ("antisymmetric",["coreflexive"]),
  
        ("antisymmetric",["reflexive_neg","right_euclidean"]),
  
        ("antisymmetric",["coreflexive_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("antisymmetric",["symmetric","transitive","serial_neg","right_euclidean_neg"]),
  
        ("antisymmetric",["transitive","serial_neg_flip","right_euclidean_neg"]),
  
        ("antisymmetric",["transitive","reflexive_neg"]),
  
        ("antisymmetric",["reflexive_neg","right_euclidean_flip"]),
  
        ("antisymmetric",["serial_neg","coreflexive_neg","right_euclidean"]),
  
        ("antisymmetric",["serial_neg","coreflexive_neg","right_euclidean_flip"]),
  
        ("antisymmetric",["transitive_neg","right_euclidean","serial_neg_flip"]),
  
        ("antisymmetric",["transitive","serial_neg","coreflexive_neg"]),
  
        ("antisymmetric",["symmetric","transitive","serial_neg","antisymmetric_neg"]),
  
        ("antisymmetric",["coreflexive_neg","right_euclidean","serial_neg_flip"]),
  
        ("antisymmetric",["transitive","coreflexive_neg","serial_neg_flip"]),
  
        ("antisymmetric",["symmetric","serial_neg","right_euclidean","antisymmetric_neg"]),
  
        ("antisymmetric",["symmetric","serial_neg","transitive_neg","right_euclidean"]),
  
        ("antisymmetric",["serial_neg","right_euclidean","right_euclidean_neg_flip"]),
  
        ("antisymmetric",["transitive","serial_neg","right_euclidean_neg_flip"]),
  
        ("antisymmetric",["serial_neg_flip","right_euclidean_neg","right_euclidean_flip"]),
  
        ("antisymmetric",["symmetric","transitive","serial_neg_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric",["symmetric","serial_neg_flip","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric",["serial_neg","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric",["right_euclidean","antisymmetric_neg","serial_neg_flip"]),
  
        ("antisymmetric",["symmetric","transitive","antisymmetric_neg","serial_neg_flip"]),
  
        ("antisymmetric",["symmetric","antisymmetric_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("antisymmetric",["right_euclidean","serial_neg_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric",["symmetric","transitive","transitive_neg","serial_neg_flip"]),
  
        ("antisymmetric",["symmetric","transitive","serial_neg","transitive_neg"]),
  
        ("antisymmetric",["serial_neg","right_euclidean_neg","right_euclidean_flip"]),
  
        ("antisymmetric",["symmetric","serial_neg","right_euclidean","right_euclidean_neg"]),
  
        ("antisymmetric",["right_euclidean","serial_neg_flip","right_euclidean_neg"]),
  
        ("antisymmetric",["symmetric","transitive_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("antisymmetric",["serial_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("antisymmetric",["serial_neg","transitive_neg","right_euclidean_flip"]),
  
        ("total",["serial","coreflexive","coreflexive_neg"]),
  
        ("total",["serial","antisymmetric","coreflexive_neg"]),
  
        ("total",["coreflexive","serial_flip","right_euclidean_neg_flip"]),
  
        ("total",["reflexive","coreflexive_neg"]),
  
        ("total",["coreflexive","serial_flip","coreflexive_neg"]),
  
        ("total",["serial","coreflexive","antisymmetric_neg"]),
  
        ("total",["serial","symmetric","antisymmetric","antisymmetric_neg"]),
  
        ("total",["antisymmetric","serial_flip","coreflexive_neg"]),
  
        ("total",["reflexive","antisymmetric_neg"]),
  
        ("total",["reflexive","right_euclidean_neg"]),
  
        ("total",["serial_flip","coreflexive_neg","right_euclidean"]),
  
        ("total",["symmetric","antisymmetric","serial_flip","right_euclidean_neg_flip"]),
  
        ("total",["transitive","serial_flip","right_euclidean_neg"]),
  
        ("total",["reflexive","right_euclidean_neg_flip"]),
  
        ("total",["reflexive","transitive_neg"]),
  
        ("total",["serial","coreflexive","right_euclidean_neg"]),
  
        ("total",["serial","coreflexive","right_euclidean_neg_flip"]),
  
        ("total",["coreflexive","serial_flip","antisymmetric_neg"]),
  
        ("total",["serial_flip","transitive_neg","right_euclidean"]),
  
        ("total",["serial","coreflexive_neg","right_euclidean_flip"]),
  
        ("total",["serial","antisymmetric_neg","right_euclidean_flip"]),
  
        ("total",["symmetric","antisymmetric","serial_flip","antisymmetric_neg"]),
  
        ("total",["symmetric","transitive","serial_flip","antisymmetric_neg"]),
  
        ("total",["serial","symmetric","antisymmetric","transitive_neg"]),
  
        ("total",["symmetric","serial_flip","antisymmetric_neg","right_euclidean_flip"]),
  
        ("total",["serial","symmetric","transitive","right_euclidean_neg"]),
  
        ("total",["serial","symmetric","transitive_neg","right_euclidean"]),
  
        ("total",["serial_flip","right_euclidean","right_euclidean_neg"]),
  
        ("total",["transitive","serial_flip","coreflexive_neg"]),
  
        ("total",["serial","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("total",["symmetric","transitive","serial_flip","transitive_neg"]),
  
        ("total",["serial","coreflexive","transitive_neg"]),
  
        ("total",["coreflexive","serial_flip","transitive_neg"]),
  
        ("total",["serial","symmetric","antisymmetric","right_euclidean_neg"]),
  
        ("total",["serial_flip","right_euclidean","right_euclidean_neg_flip"]),
  
        ("total",["antisymmetric","serial_flip","right_euclidean_neg"]),
  
        ("total",["serial","symmetric","right_euclidean","antisymmetric_neg"]),
  
        ("total",["serial","symmetric","transitive","antisymmetric_neg"]),
  
        ("total",["serial","transitive","coreflexive_neg"]),
  
        ("total",["serial_flip","right_euclidean","antisymmetric_neg"]),
  
        ("total",["serial","antisymmetric","right_euclidean_neg_flip"]),
  
        ("total",["serial","transitive_neg","right_euclidean_flip"]),
  
        ("total",["serial","right_euclidean_neg","right_euclidean_flip"]),
  
        ("total",["serial_flip","right_euclidean_neg","right_euclidean_flip"]),
  
        ("total",["symmetric","antisymmetric","serial_flip","transitive_neg"]),
  
        ("total",["symmetric","serial_flip","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("total",["serial","symmetric","right_euclidean","right_euclidean_neg"]),
  
        ("total",["serial","transitive","right_euclidean_neg_flip"]),
  
        ("total",["coreflexive","serial_flip","right_euclidean_neg"]),
  
        ("total",["symmetric","transitive","serial_flip","right_euclidean_neg_flip"]),
  
        ("total",["serial","symmetric","transitive","transitive_neg"]),
  
        ("total",["serial","coreflexive_neg","right_euclidean"]),
  
        ("total",["serial_flip","coreflexive_neg","right_euclidean_flip"]),
  
        ("total",["symmetric","serial_flip","transitive_neg","right_euclidean_flip"]),
  
        ("total",["serial","right_euclidean","right_euclidean_neg_flip"]),
  
        ("serial",["reflexive"]),
  
        ("serial",["total"]),
  
        ("serial",["serial_flip","coreflexive_neg"]),
  
        ("serial",["coreflexive","serial_flip"]),
  
        ("serial",["symmetric","serial_flip"]),
  
        ("serial",["serial_flip","right_euclidean"]),
  
        ("serial",["serial_flip","right_euclidean_neg"]),
  
        ("serial",["serial_flip","reflexive_neg","right_euclidean_neg_flip"]),
  
        ("serial",["serial_neg","serial_flip","right_euclidean_neg_flip"]),
        ("transitive_neg",["total_neg","symmetric_neg"]),
  
        ("transitive_neg",["symmetric_neg","total"]),
  
        ("transitive_neg",["antisymmetric_neg","coreflexive"]),
  
        ("transitive_neg",["coreflexive_neg"]),
  
        ("transitive_neg",["transitive","right_euclidean_neg"]),
  
        ("transitive_neg",["reflexive_neg","right_euclidean"]),
  
        ("transitive_neg",["reflexive","right_euclidean_neg"]),
  
        ("transitive_neg",["antisymmetric_neg","transitive"]),
  
        ("transitive_neg",["total_neg","right_euclidean"]),
  
        ("transitive_neg",["reflexive_neg","coreflexive"]),
  
        ("transitive_neg",["total","coreflexive"]),
  
        ("transitive_neg",["total","right_euclidean_neg"]),
  
        ("transitive_neg",["symmetric_neg","antisymmetric_neg"]),
  
        ("transitive_neg",["symmetric_neg","right_euclidean_neg"]),
  
        ("transitive_neg",["reflexive_neg","symmetric_neg","antisymmetric"]),
  
        ("transitive_neg",["right_euclidean_neg","right_euclidean"]),
  
        ("transitive_neg",["total_neg","right_euclidean_neg"]),
  
        ("transitive_neg",["total_neg","coreflexive"]),
  
        ("transitive_neg",["total","right_euclidean"]),
  
        ("transitive_neg",["total","transitive"]),
  
        ("transitive_neg",["reflexive_neg","right_euclidean_flip"]),
  
        ("transitive_neg",["symmetric_neg","right_euclidean_neg_flip"]),
  
        ("transitive_neg",["reflexive_neg","right_euclidean_neg"]),
  
        ("transitive_neg",["antisymmetric_neg","right_euclidean"]),
  
        ("transitive_neg",["antisymmetric_neg","right_euclidean_neg"]),
  
        ("transitive_neg",["total","right_euclidean_neg_flip"]),
  
        ("transitive_neg",["coreflexive","right_euclidean_neg_flip"]),
  
        ("transitive_neg",["reflexive_neg","right_euclidean_neg_flip"]),
  
        ("transitive_neg",["total_neg","right_euclidean_neg_flip"]),
  
        ("transitive_neg",["serial_neg_flip","right_euclidean_neg"]),
  
        ("transitive_neg",["total_neg","right_euclidean_flip"]),
  
        ("transitive_neg",["total","right_euclidean_flip"]),
  
        ("transitive_neg",["coreflexive","right_euclidean_neg"]),
  
        ("transitive_neg",["right_euclidean_neg","antisymmetric"]),
  
        ("transitive_neg",["right_euclidean_neg","right_euclidean_neg_flip"]),
  
        ("transitive_neg",["reflexive_neg","symmetric_neg","transitive"]),
  
        ("transitive_neg",["antisymmetric_neg","right_euclidean_flip"]),
  
        ("transitive_neg",["transitive","right_euclidean_neg_flip"]),
  
        ("transitive_neg",["antisymmetric_neg","right_euclidean_neg_flip"]),
  
        ("transitive_neg",["serial_neg","right_euclidean_neg_flip"]),
  
        ("transitive_neg",["antisymmetric","right_euclidean_neg_flip"]),
  
        ("transitive_neg",["right_euclidean_neg_flip","right_euclidean_flip"]),
  
        ("transitive_neg",["reflexive","right_euclidean_neg_flip"]),
  
        ("transitive_neg",["right_euclidean_neg","right_euclidean_flip"]),
  
        ("transitive_neg",["right_euclidean","right_euclidean_neg_flip"]),
  
        ("reflexive_neg",["serial_neg","antisymmetric_neg","coreflexive"]),
  
        ("reflexive_neg",["antisymmetric_neg","serial_neg_flip","coreflexive"]),
  
        ("reflexive_neg",["total_neg"]),
  
        ("reflexive_neg",["transitive_neg","serial_neg_flip","right_euclidean"]),
  
        ("reflexive_neg",["serial_neg","coreflexive_neg"]),
  
        ("reflexive_neg",["serial_neg_flip","right_euclidean_neg"]),
  
        ("reflexive_neg",["symmetric_neg","transitive_neg","serial_neg_flip"]),
  
        ("reflexive_neg",["symmetric_neg","serial_neg_flip","right_euclidean_neg_flip"]),
  
        ("reflexive_neg",["serial_neg","symmetric_neg","right_euclidean_neg"]),
  
        ("reflexive_neg",["serial_neg","right_euclidean_neg_flip"]),
  
        ("reflexive_neg",["serial_neg","symmetric_neg","transitive_neg"]),
  
        ("reflexive_neg",["transitive_neg","serial_neg_flip","coreflexive"]),
  
        ("reflexive_neg",["coreflexive_neg","serial_neg_flip"]),
  
        ("reflexive_neg",["serial_neg","symmetric_neg","antisymmetric_neg"]),
  
        ("reflexive_neg",["serial_neg","transitive_neg","right_euclidean_flip"]),
  
        ("reflexive_neg",["serial_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("reflexive_neg",["symmetric_neg","antisymmetric_neg","serial_neg_flip"]),
  
        ("reflexive_neg",["serial_neg_flip","right_euclidean","right_euclidean_neg_flip"]),
  
        ("reflexive_neg",["antisymmetric_neg","serial_neg_flip","right_euclidean"]),
  
        ("reflexive_neg",["serial_neg","transitive_neg","coreflexive"]),
  
        ("reflexive_neg",["serial_neg","right_euclidean_neg","right_euclidean_flip"]),
  
        ("reflexive_neg",["serial_neg","coreflexive","right_euclidean_neg"]),
  
        ("reflexive_neg",["serial_neg_flip","coreflexive","right_euclidean_neg_flip"]),
  
        ("symmetric_neg",["transitive_neg","serial_neg_flip","right_euclidean"]),
  
        ("symmetric_neg",["coreflexive_neg"]),
  
        ("symmetric_neg",["reflexive","right_euclidean_neg"]),
  
        ("symmetric_neg",["reflexive","right_euclidean_neg_flip"]),
  
        ("symmetric_neg",["coreflexive"]),
  
        ("symmetric_neg",["reflexive_neg","right_euclidean"]),
  
        ("symmetric_neg",["reflexive_neg","right_euclidean_flip"]),
  
        ("symmetric_neg",["reflexive_neg","right_euclidean_neg"]),
  
        ("symmetric_neg",["total","right_euclidean_neg"]),
  
        ("symmetric_neg",["transitive","right_euclidean_neg","serial_flip"]),
  
        ("symmetric_neg",["reflexive","right_euclidean"]),
  
        ("symmetric_neg",["total","right_euclidean"]),
  
        ("symmetric_neg",["total_neg","right_euclidean"]),
  
        ("symmetric_neg",["total_neg","right_euclidean_neg"]),
  
        ("symmetric_neg",["total_neg","right_euclidean_neg_flip"]),
  
        ("symmetric_neg",["total_neg","right_euclidean_flip"]),
  
        ("symmetric_neg",["serial_neg_flip","right_euclidean_neg"]),
  
        ("symmetric_neg",["total","right_euclidean_neg_flip"]),
  
        ("symmetric_neg",["reflexive","right_euclidean_flip"]),
  
        ("symmetric_neg",["right_euclidean_neg","right_euclidean_flip"]),
  
        ("symmetric_neg",["reflexive_neg","right_euclidean_neg_flip"]),
  
        ("symmetric_neg",["right_euclidean","right_euclidean_neg_flip"]),
  
        ("symmetric_neg",["right_euclidean_neg","right_euclidean_neg_flip"]),
  
        ("symmetric_neg",["serial","transitive","right_euclidean_neg_flip"]),
  
        ("symmetric_neg",["serial","right_euclidean_flip"]),
  
        ("symmetric_neg",["right_euclidean","right_euclidean_flip"]),
  
        ("symmetric_neg",["serial_neg","transitive_neg","right_euclidean_flip"]),
  
        ("symmetric_neg",["antisymmetric_neg","serial_neg_flip","right_euclidean"]),
  
        ("symmetric_neg",["serial_flip","right_euclidean"]),
  
        ("symmetric_neg",["serial_neg","right_euclidean_neg_flip"]),
  
        ("symmetric_neg",["serial_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("symmetric_neg",["total","right_euclidean_flip"]),
  
        ("symmetric_neg",["right_euclidean_neg","antisymmetric","serial_flip"]),
  
        ("symmetric_neg",["serial","antisymmetric","right_euclidean_neg_flip"]),
  
        ("right_euclidean_neg",["symmetric_neg","total"]),
  
        ("right_euclidean_neg",["total_neg","symmetric_neg"]),
  
        ("right_euclidean_neg",["antisymmetric_neg","coreflexive"]),
  
        ("right_euclidean_neg",["transitive_neg","right_euclidean"]),
  
        ("right_euclidean_neg",["reflexive_neg","right_euclidean"]),
  
        ("right_euclidean_neg",["reflexive_neg","coreflexive"]),
  
        ("right_euclidean_neg",["total","right_euclidean"]),
  
        ("right_euclidean_neg",["coreflexive_neg"]),
  
        ("right_euclidean_neg",["reflexive","right_euclidean_neg_flip"]),
  
        ("right_euclidean_neg",["total_neg","coreflexive"]),
  
        ("right_euclidean_neg",["total_neg","right_euclidean"]),
  
        ("right_euclidean_neg",["transitive_neg","reflexive","right_euclidean_flip"]),
  
        ("right_euclidean_neg",["reflexive_neg","right_euclidean_flip"]),
  
        ("right_euclidean_neg",["transitive_neg","coreflexive"]),
  
        ("right_euclidean_neg",["total","right_euclidean_neg_flip"]),
  
        ("right_euclidean_neg",["serial","right_euclidean_neg_flip","right_euclidean_flip"]),
  
        ("right_euclidean_neg",["symmetric_neg","antisymmetric_neg"]),
  
        ("right_euclidean_neg",["total_neg","right_euclidean_neg_flip"]),
  
        ("right_euclidean_neg",["serial","transitive","right_euclidean_neg_flip"]),
  
        ("right_euclidean_neg",["transitive_neg","serial","right_euclidean_flip"]),
  
        ("right_euclidean_neg",["total","right_euclidean_flip"]),
  
        ("right_euclidean_neg",["antisymmetric_neg","right_euclidean"]),
  
        ("right_euclidean_neg",["symmetric_neg","transitive_neg"]),
  
        ("right_euclidean_neg",["total_neg","right_euclidean_flip"]),
  
        ("right_euclidean_neg",["reflexive_neg","symmetric_neg","antisymmetric"]),
  
        ("right_euclidean_neg",["serial_neg","transitive_neg","right_euclidean_flip"]),
  
        ("right_euclidean_neg",["reflexive_neg","right_euclidean_neg_flip"]),
  
        ("right_euclidean_neg",["right_euclidean","right_euclidean_neg_flip"]),
  
        ("right_euclidean_neg",["total","coreflexive"]),
  
        ("right_euclidean_neg",["antisymmetric_neg","reflexive","right_euclidean_flip"]),
  
        ("right_euclidean_neg",["antisymmetric_neg","serial","right_euclidean_flip"]),
  
        ("right_euclidean_neg",["serial_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("right_euclidean_neg",["reflexive_neg","symmetric_neg","transitive"]),
  
        ("right_euclidean_neg",["serial_neg","right_euclidean_neg_flip"]),
  
        ("right_euclidean_neg",["symmetric_neg","right_euclidean_neg_flip"]),
  
        ("right_euclidean_neg",["serial","antisymmetric","right_euclidean_neg_flip"]),
  
        ("right_euclidean_neg",["coreflexive","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["antisymmetric_neg","coreflexive"]),
  
        ("coreflexive_neg",["symmetric_neg","total"]),
  
        ("coreflexive_neg",["transitive_neg","reflexive","right_euclidean"]),
  
        ("coreflexive_neg",["reflexive_neg","antisymmetric_neg","right_euclidean"]),
  
        ("coreflexive_neg",["symmetric_neg","antisymmetric_neg"]),
  
        ("coreflexive_neg",["reflexive","right_euclidean_neg"]),
  
        ("coreflexive_neg",["reflexive","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["total_neg","antisymmetric_neg","right_euclidean_neg"]),
  
        ("coreflexive_neg",["reflexive_neg","antisymmetric_neg","right_euclidean_neg"]),
  
        ("coreflexive_neg",["reflexive_neg","antisymmetric_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["transitive","right_euclidean_neg","serial_flip"]),
  
        ("coreflexive_neg",["total","right_euclidean_neg"]),
  
        ("coreflexive_neg",["total_neg","antisymmetric_neg","right_euclidean"]),
  
        ("coreflexive_neg",["total_neg","antisymmetric_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["reflexive_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("coreflexive_neg",["serial","coreflexive","right_euclidean_neg"]),
  
        ("coreflexive_neg",["serial","coreflexive","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["coreflexive","right_euclidean_neg","serial_flip"]),
  
        ("coreflexive_neg",["transitive_neg","serial_flip","right_euclidean"]),
  
        ("coreflexive_neg",["coreflexive","serial_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["total","coreflexive"]),
  
        ("coreflexive_neg",["antisymmetric_neg","right_euclidean_neg","right_euclidean_flip"]),
  
        ("coreflexive_neg",["antisymmetric_neg","serial_flip","right_euclidean"]),
  
        ("coreflexive_neg",["antisymmetric_neg","serial_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive_neg",["total","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["total_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("coreflexive_neg",["antisymmetric_neg","right_euclidean","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["serial","right_euclidean_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive_neg",["symmetric_neg","transitive_neg","serial_flip","right_euclidean_flip"]),
  
        ("coreflexive_neg",["symmetric_neg","antisymmetric","serial_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["transitive_neg","serial","coreflexive"]),
  
        ("coreflexive_neg",["serial","transitive","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["antisymmetric_neg","reflexive","right_euclidean_flip"]),
  
        ("coreflexive_neg",["symmetric_neg","transitive_neg","reflexive"]),
  
        ("coreflexive_neg",["antisymmetric_neg","right_euclidean_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["symmetric_neg","transitive_neg","serial","right_euclidean"]),
  
        ("coreflexive_neg",["transitive_neg","reflexive","coreflexive"]),
  
        ("coreflexive_neg",["transitive_neg","reflexive","right_euclidean_flip"]),
  
        ("coreflexive_neg",["antisymmetric_neg","reflexive","right_euclidean"]),
  
        ("coreflexive_neg",["total","right_euclidean_flip"]),
  
        ("coreflexive_neg",["total","right_euclidean"]),
  
        ("coreflexive_neg",["antisymmetric_neg","right_euclidean","right_euclidean_flip"]),
  
        ("coreflexive_neg",["serial_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("coreflexive_neg",["serial_neg","antisymmetric_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["serial_flip","right_euclidean","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["transitive_neg","coreflexive","serial_flip"]),
  
        ("coreflexive_neg",["symmetric_neg","transitive_neg","serial","antisymmetric"]),
  
        ("coreflexive_neg",["serial","right_euclidean_neg","right_euclidean_flip"]),
  
        ("coreflexive_neg",["right_euclidean_neg","serial_flip","right_euclidean_flip"]),
  
        ("coreflexive_neg",["right_euclidean_neg","antisymmetric","serial_flip"]),
  
        ("coreflexive_neg",["symmetric_neg","serial","transitive","right_euclidean_neg"]),
  
        ("coreflexive_neg",["symmetric_neg","serial","right_euclidean_neg","antisymmetric"]),
  
        ("coreflexive_neg",["symmetric_neg","serial","right_euclidean_neg","right_euclidean"]),
  
        ("coreflexive_neg",["transitive_neg","serial","right_euclidean_flip"]),
  
        ("coreflexive_neg",["symmetric_neg","transitive_neg","serial","transitive"]),
  
        ("coreflexive_neg",["symmetric_neg","transitive_neg","antisymmetric","serial_flip"]),
  
        ("coreflexive_neg",["symmetric_neg","serial_flip","right_euclidean_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive_neg",["symmetric_neg","transitive_neg","transitive","serial_flip"]),
  
        ("coreflexive_neg",["antisymmetric_neg","serial","right_euclidean_flip"]),
  
        ("coreflexive_neg",["antisymmetric_neg","serial_neg_flip","right_euclidean"]),
  
        ("coreflexive_neg",["symmetric_neg","transitive","serial_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["serial","antisymmetric","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg",["right_euclidean_neg","serial_flip","right_euclidean"]),
  
        ("coreflexive_neg",["serial","right_euclidean","right_euclidean_neg_flip"]),
  
        ("antisymmetric_neg",["total"]),
  
        ("antisymmetric_neg",["coreflexive_neg"]),
  
        ("antisymmetric_neg",["reflexive","right_euclidean_neg"]),
  
        ("antisymmetric_neg",["coreflexive","serial_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric_neg",["symmetric_neg","transitive_neg","serial","right_euclidean"]),
  
        ("antisymmetric_neg",["transitive_neg","serial_flip","right_euclidean"]),
  
        ("antisymmetric_neg",["transitive_neg","reflexive"]),
  
        ("antisymmetric_neg",["reflexive","right_euclidean_neg_flip"]),
  
        ("antisymmetric_neg",["serial","coreflexive","right_euclidean_neg"]),
  
        ("antisymmetric_neg",["serial","coreflexive","right_euclidean_neg_flip"]),
  
        ("antisymmetric_neg",["transitive","right_euclidean_neg","serial_flip"]),
  
        ("antisymmetric_neg",["transitive_neg","serial","coreflexive"]),
  
        ("antisymmetric_neg",["symmetric_neg","transitive_neg","serial","antisymmetric"]),
  
        ("antisymmetric_neg",["coreflexive","right_euclidean_neg","serial_flip"]),
  
        ("antisymmetric_neg",["transitive_neg","coreflexive","serial_flip"]),
  
        ("antisymmetric_neg",["symmetric_neg","serial","right_euclidean_neg","antisymmetric"]),
  
        ("antisymmetric_neg",["symmetric_neg","serial","transitive","right_euclidean_neg"]),
  
        ("antisymmetric_neg",["serial","right_euclidean_neg","right_euclidean_flip"]),
  
        ("antisymmetric_neg",["transitive_neg","serial","right_euclidean_flip"]),
  
        ("antisymmetric_neg",["serial_flip","right_euclidean","right_euclidean_neg_flip"]),
  
        ("antisymmetric_neg",["symmetric_neg","transitive_neg","serial_flip","right_euclidean_flip"]),
  
        ("antisymmetric_neg",["symmetric_neg","serial_flip","right_euclidean_neg_flip","right_euclidean_flip"]),
  
        ("antisymmetric_neg",["serial","right_euclidean_neg_flip","right_euclidean_flip"]),
  
        ("antisymmetric_neg",["right_euclidean_neg","antisymmetric","serial_flip"]),
  
        ("antisymmetric_neg",["symmetric_neg","transitive_neg","antisymmetric","serial_flip"]),
  
        ("antisymmetric_neg",["symmetric_neg","antisymmetric","serial_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric_neg",["right_euclidean_neg","serial_flip","right_euclidean_flip"]),
  
        ("antisymmetric_neg",["symmetric_neg","transitive_neg","transitive","serial_flip"]),
  
        ("antisymmetric_neg",["symmetric_neg","transitive_neg","serial","transitive"]),
  
        ("antisymmetric_neg",["serial","right_euclidean","right_euclidean_neg_flip"]),
  
        ("antisymmetric_neg",["symmetric_neg","serial","right_euclidean_neg","right_euclidean"]),
  
        ("antisymmetric_neg",["right_euclidean_neg","serial_flip","right_euclidean"]),
  
        ("antisymmetric_neg",["symmetric_neg","transitive","serial_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric_neg",["serial","antisymmetric","right_euclidean_neg_flip"]),
  
        ("antisymmetric_neg",["serial","transitive","right_euclidean_neg_flip"]),
  
        ("total_neg",["serial_neg","coreflexive_neg","coreflexive"]),
  
        ("total_neg",["serial_neg","antisymmetric_neg","coreflexive"]),
  
        ("total_neg",["coreflexive_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("total_neg",["reflexive_neg","coreflexive"]),
  
        ("total_neg",["coreflexive_neg","serial_neg_flip","coreflexive"]),
  
        ("total_neg",["serial_neg","coreflexive_neg","antisymmetric"]),
  
        ("total_neg",["serial_neg","symmetric_neg","antisymmetric_neg","antisymmetric"]),
  
        ("total_neg",["antisymmetric_neg","serial_neg_flip","coreflexive"]),
  
        ("total_neg",["reflexive_neg","antisymmetric"]),
  
        ("total_neg",["reflexive_neg","right_euclidean"]),
  
        ("total_neg",["serial_neg_flip","coreflexive","right_euclidean_neg"]),
  
        ("total_neg",["symmetric_neg","antisymmetric_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("total_neg",["transitive_neg","serial_neg_flip","right_euclidean"]),
  
        ("total_neg",["reflexive_neg","right_euclidean_flip"]),
  
        ("total_neg",["reflexive_neg","transitive"]),
  
        ("total_neg",["serial_neg","coreflexive_neg","right_euclidean"]),
  
        ("total_neg",["serial_neg","coreflexive_neg","right_euclidean_flip"]),
  
        ("total_neg",["coreflexive_neg","serial_neg_flip","antisymmetric"]),
  
        ("total_neg",["serial_neg_flip","transitive","right_euclidean_neg"]),
  
        ("total_neg",["serial_neg","coreflexive","right_euclidean_neg_flip"]),
  
        ("total_neg",["serial_neg","antisymmetric","right_euclidean_neg_flip"]),
  
        ("total_neg",["symmetric_neg","antisymmetric_neg","serial_neg_flip","antisymmetric"]),
  
        ("total_neg",["symmetric_neg","transitive_neg","serial_neg_flip","antisymmetric"]),
  
        ("total_neg",["serial_neg","symmetric_neg","antisymmetric_neg","transitive"]),
  
        ("total_neg",["symmetric_neg","serial_neg_flip","antisymmetric","right_euclidean_neg_flip"]),
  
        ("total_neg",["serial_neg","symmetric_neg","transitive_neg","right_euclidean"]),
  
        ("total_neg",["serial_neg","symmetric_neg","transitive","right_euclidean_neg"]),
  
        ("total_neg",["serial_neg_flip","right_euclidean_neg","right_euclidean"]),
  
        ("total_neg",["transitive_neg","serial_neg_flip","coreflexive"]),
  
        ("total_neg",["serial_neg","right_euclidean_neg_flip","right_euclidean_flip"]),
  
        ("total_neg",["symmetric_neg","transitive_neg","serial_neg_flip","transitive"]),
  
        ("total_neg",["serial_neg","coreflexive_neg","transitive"]),
  
        ("total_neg",["coreflexive_neg","serial_neg_flip","transitive"]),
  
        ("total_neg",["serial_neg","symmetric_neg","antisymmetric_neg","right_euclidean"]),
  
        ("total_neg",["serial_neg_flip","right_euclidean_neg","right_euclidean_flip"]),
  
        ("total_neg",["antisymmetric_neg","serial_neg_flip","right_euclidean"]),
  
        ("total_neg",["serial_neg","symmetric_neg","right_euclidean_neg","antisymmetric"]),
  
        ("total_neg",["serial_neg","symmetric_neg","transitive_neg","antisymmetric"]),
  
        ("total_neg",["serial_neg","transitive_neg","coreflexive"]),
  
        ("total_neg",["serial_neg_flip","right_euclidean_neg","antisymmetric"]),
  
        ("total_neg",["serial_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("total_neg",["serial_neg","transitive","right_euclidean_neg_flip"]),
  
        ("total_neg",["serial_neg","right_euclidean","right_euclidean_neg_flip"]),
  
        ("total_neg",["serial_neg_flip","right_euclidean","right_euclidean_neg_flip"]),
  
        ("total_neg",["symmetric_neg","antisymmetric_neg","serial_neg_flip","transitive"]),
  
        ("total_neg",["symmetric_neg","serial_neg_flip","right_euclidean_neg_flip","right_euclidean_flip"]),
  
        ("total_neg",["serial_neg","symmetric_neg","right_euclidean_neg","right_euclidean"]),
  
        ("total_neg",["serial_neg","transitive_neg","right_euclidean_flip"]),
  
        ("total_neg",["coreflexive_neg","serial_neg_flip","right_euclidean"]),
  
        ("total_neg",["symmetric_neg","transitive_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("total_neg",["serial_neg","symmetric_neg","transitive_neg","transitive"]),
  
        ("total_neg",["serial_neg","coreflexive","right_euclidean_neg"]),
  
        ("total_neg",["serial_neg_flip","coreflexive","right_euclidean_neg_flip"]),
  
        ("total_neg",["symmetric_neg","serial_neg_flip","transitive","right_euclidean_neg_flip"]),
  
        ("total_neg",["serial_neg","right_euclidean_neg","right_euclidean_flip"]),
  
        ("serial_neg",["reflexive_neg"]),
  
        ("serial_neg",["total_neg"]),
  
        ("serial_neg",["serial_neg_flip","coreflexive"]),
  
        ("serial_neg",["coreflexive_neg","serial_neg_flip"]),
  
        ("serial_neg",["symmetric_neg","serial_neg_flip"]),
  
        ("serial_neg",["serial_neg_flip","right_euclidean_neg"]),
  
        ("serial_neg",["serial_neg_flip","right_euclidean"]),
  
        ("serial_neg",["serial_neg_flip","reflexive","right_euclidean_flip"]),
  
        ("serial_neg",["serial","serial_neg_flip","right_euclidean_flip"]),
        ("transitive_flip",["total_flip","symmetric_flip"]),
  
        ("transitive_flip",["symmetric_flip","total_neg_flip"]),
  
        ("transitive_flip",["antisymmetric_flip","coreflexive_neg_flip"]),
  
        ("transitive_flip",["coreflexive_flip"]),
  
        ("transitive_flip",["transitive_neg_flip","right_euclidean_flip"]),
  
        ("transitive_flip",["reflexive_flip","right_euclidean_neg_flip"]),
  
        ("transitive_flip",["reflexive_neg_flip","right_euclidean_flip"]),
  
        ("transitive_flip",["antisymmetric_flip","transitive_neg_flip"]),
  
        ("transitive_flip",["total_flip","right_euclidean_neg_flip"]),
  
        ("transitive_flip",["reflexive_flip","coreflexive_neg_flip"]),
  
        ("transitive_flip",["total_neg_flip","coreflexive_neg_flip"]),
  
        ("transitive_flip",["total_neg_flip","right_euclidean_flip"]),
  
        ("transitive_flip",["symmetric_flip","antisymmetric_flip"]),
  
        ("transitive_flip",["symmetric_flip","right_euclidean_flip"]),
  
        ("transitive_flip",["reflexive_flip","symmetric_flip","antisymmetric_neg_flip"]),
  
        ("transitive_flip",["right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("transitive_flip",["total_flip","right_euclidean_flip"]),
  
        ("transitive_flip",["total_flip","coreflexive_neg_flip"]),
  
        ("transitive_flip",["total_neg_flip","right_euclidean_neg_flip"]),
  
        ("transitive_flip",["total_neg_flip","transitive_neg_flip"]),
  
        ("transitive_flip",["reflexive_flip","right_euclidean_neg"]),
  
        ("transitive_flip",["symmetric_flip","right_euclidean"]),
  
        ("transitive_flip",["reflexive_flip","right_euclidean_flip"]),
  
        ("transitive_flip",["antisymmetric_flip","right_euclidean_neg_flip"]),
  
        ("transitive_flip",["antisymmetric_flip","right_euclidean_flip"]),
  
        ("transitive_flip",["total_neg_flip","right_euclidean"]),
  
        ("transitive_flip",["coreflexive_neg_flip","right_euclidean"]),
  
        ("transitive_flip",["reflexive_flip","right_euclidean"]),
  
        ("transitive_flip",["total_flip","right_euclidean"]),
  
        ("transitive_flip",["serial","right_euclidean_flip"]),
  
        ("transitive_flip",["total_flip","right_euclidean_neg"]),
  
        ("transitive_flip",["total_neg_flip","right_euclidean_neg"]),
  
        ("transitive_flip",["coreflexive_neg_flip","right_euclidean_flip"]),
  
        ("transitive_flip",["right_euclidean_flip","antisymmetric_neg_flip"]),
  
        ("transitive_flip",["right_euclidean_flip","right_euclidean"]),
  
        ("transitive_flip",["reflexive_flip","symmetric_flip","transitive_neg_flip"]),
  
        ("transitive_flip",["antisymmetric_flip","right_euclidean_neg"]),
  
        ("transitive_flip",["transitive_neg_flip","right_euclidean"]),
  
        ("transitive_flip",["antisymmetric_flip","right_euclidean"]),
  
        ("transitive_flip",["serial_flip","right_euclidean"]),
  
        ("transitive_flip",["antisymmetric_neg_flip","right_euclidean"]),
  
        ("transitive_flip",["right_euclidean","right_euclidean_neg"]),
  
        ("transitive_flip",["reflexive_neg_flip","right_euclidean"]),
  
        ("transitive_flip",["right_euclidean_flip","right_euclidean_neg"]),
  
        ("transitive_flip",["right_euclidean_neg_flip","right_euclidean"]),
  
        ("reflexive_flip",["serial_flip","antisymmetric_flip","coreflexive_neg_flip"]),
  
        ("reflexive_flip",["antisymmetric_flip","serial","coreflexive_neg_flip"]),
  
        ("reflexive_flip",["total_flip"]),
  
        ("reflexive_flip",["transitive_flip","serial","right_euclidean_neg_flip"]),
  
        ("reflexive_flip",["serial_flip","coreflexive_flip"]),
  
        ("reflexive_flip",["serial","right_euclidean_flip"]),
  
        ("reflexive_flip",["symmetric_flip","transitive_flip","serial"]),
  
        ("reflexive_flip",["symmetric_flip","serial","right_euclidean"]),
  
        ("reflexive_flip",["serial_flip","symmetric_flip","right_euclidean_flip"]),
  
        ("reflexive_flip",["serial_flip","right_euclidean"]),
  
        ("reflexive_flip",["serial_flip","symmetric_flip","transitive_flip"]),
  
        ("reflexive_flip",["transitive_flip","serial","coreflexive_neg_flip"]),
  
        ("reflexive_flip",["coreflexive_flip","serial"]),
  
        ("reflexive_flip",["serial_flip","symmetric_flip","antisymmetric_flip"]),
  
        ("reflexive_flip",["serial_flip","transitive_flip","right_euclidean_neg"]),
  
        ("reflexive_flip",["serial_flip","antisymmetric_flip","right_euclidean_neg"]),
  
        ("reflexive_flip",["symmetric_flip","antisymmetric_flip","serial"]),
  
        ("reflexive_flip",["serial","right_euclidean_neg_flip","right_euclidean"]),
  
        ("reflexive_flip",["antisymmetric_flip","serial","right_euclidean_neg_flip"]),
  
        ("reflexive_flip",["serial_flip","transitive_flip","coreflexive_neg_flip"]),
  
        ("reflexive_flip",["serial_flip","right_euclidean_flip","right_euclidean_neg"]),
  
        ("reflexive_flip",["serial_flip","coreflexive_neg_flip","right_euclidean_flip"]),
  
        ("reflexive_flip",["serial","coreflexive_neg_flip","right_euclidean"]),
  
        ("symmetric_flip",["transitive_flip","serial","right_euclidean_neg_flip"]),
  
        ("symmetric_flip",["coreflexive_flip"]),
  
        ("symmetric_flip",["reflexive_neg_flip","right_euclidean_flip"]),
  
        ("symmetric_flip",["reflexive_neg_flip","right_euclidean"]),
  
        ("symmetric_flip",["coreflexive_neg_flip"]),
  
        ("symmetric_flip",["reflexive_flip","right_euclidean_neg_flip"]),
  
        ("symmetric_flip",["reflexive_flip","right_euclidean_neg"]),
  
        ("symmetric_flip",["reflexive_flip","right_euclidean_flip"]),
  
        ("symmetric_flip",["total_neg_flip","right_euclidean_flip"]),
  
        ("symmetric_flip",["transitive_neg_flip","right_euclidean_flip","serial_neg"]),
  
        ("symmetric_flip",["reflexive_neg_flip","right_euclidean_neg_flip"]),
  
        ("symmetric_flip",["total_neg_flip","right_euclidean_neg_flip"]),
  
        ("symmetric_flip",["total_flip","right_euclidean_neg_flip"]),
  
        ("symmetric_flip",["total_flip","right_euclidean_flip"]),
  
        ("symmetric_flip",["total_flip","right_euclidean"]),
  
        ("symmetric_flip",["total_flip","right_euclidean_neg"]),
  
        ("symmetric_flip",["serial","right_euclidean_flip"]),
  
        ("symmetric_flip",["total_neg_flip","right_euclidean"]),
  
        ("symmetric_flip",["reflexive_neg_flip","right_euclidean_neg"]),
  
        ("symmetric_flip",["right_euclidean_flip","right_euclidean_neg"]),
  
        ("symmetric_flip",["reflexive_flip","right_euclidean"]),
  
        ("symmetric_flip",["right_euclidean_neg_flip","right_euclidean"]),
  
        ("symmetric_flip",["right_euclidean_flip","right_euclidean"]),
  
        ("symmetric_flip",["serial_neg_flip","transitive_neg_flip","right_euclidean"]),
  
        ("symmetric_flip",["serial_neg_flip","right_euclidean_neg"]),
  
        ("symmetric_flip",["right_euclidean_neg_flip","right_euclidean_neg"]),
  
        ("symmetric_flip",["serial_flip","transitive_flip","right_euclidean_neg"]),
  
        ("symmetric_flip",["antisymmetric_flip","serial","right_euclidean_neg_flip"]),
  
        ("symmetric_flip",["serial_neg","right_euclidean_neg_flip"]),
  
        ("symmetric_flip",["serial_flip","right_euclidean"]),
  
        ("symmetric_flip",["serial_flip","antisymmetric_flip","right_euclidean_neg"]),
  
        ("symmetric_flip",["total_neg_flip","right_euclidean_neg"]),
  
        ("symmetric_flip",["right_euclidean_flip","antisymmetric_neg_flip","serial_neg"]),
  
        ("symmetric_flip",["serial_neg_flip","antisymmetric_neg_flip","right_euclidean"]),
  
        ("right_euclidean_flip",["symmetric_flip","total_neg_flip"]),
  
        ("right_euclidean_flip",["total_flip","symmetric_flip"]),
  
        ("right_euclidean_flip",["antisymmetric_flip","coreflexive_neg_flip"]),
  
        ("right_euclidean_flip",["transitive_flip","right_euclidean_neg_flip"]),
  
        ("right_euclidean_flip",["reflexive_flip","right_euclidean_neg_flip"]),
  
        ("right_euclidean_flip",["reflexive_flip","coreflexive_neg_flip"]),
  
        ("right_euclidean_flip",["total_neg_flip","right_euclidean_neg_flip"]),
  
        ("right_euclidean_flip",["coreflexive_flip"]),
  
        ("right_euclidean_flip",["reflexive_neg_flip","right_euclidean"]),
  
        ("right_euclidean_flip",["total_flip","coreflexive_neg_flip"]),
  
        ("right_euclidean_flip",["total_flip","right_euclidean_neg_flip"]),
  
        ("right_euclidean_flip",["transitive_flip","reflexive_neg_flip","right_euclidean_neg"]),
  
        ("right_euclidean_flip",["reflexive_flip","right_euclidean_neg"]),
  
        ("right_euclidean_flip",["transitive_flip","coreflexive_neg_flip"]),
  
        ("right_euclidean_flip",["total_neg_flip","right_euclidean"]),
  
        ("right_euclidean_flip",["serial_neg_flip","right_euclidean","right_euclidean_neg"]),
  
        ("right_euclidean_flip",["symmetric_flip","antisymmetric_flip"]),
  
        ("right_euclidean_flip",["total_flip","right_euclidean"]),
  
        ("right_euclidean_flip",["serial_neg_flip","transitive_neg_flip","right_euclidean"]),
  
        ("right_euclidean_flip",["transitive_flip","serial_neg_flip","right_euclidean_neg"]),
  
        ("right_euclidean_flip",["total_neg_flip","right_euclidean_neg"]),
  
        ("right_euclidean_flip",["antisymmetric_flip","right_euclidean_neg_flip"]),
  
        ("right_euclidean_flip",["symmetric_flip","transitive_flip"]),
  
        ("right_euclidean_flip",["total_flip","right_euclidean_neg"]),
  
        ("right_euclidean_flip",["reflexive_flip","symmetric_flip","antisymmetric_neg_flip"]),
  
        ("right_euclidean_flip",["serial_flip","transitive_flip","right_euclidean_neg"]),
  
        ("right_euclidean_flip",["reflexive_flip","right_euclidean"]),
  
        ("right_euclidean_flip",["right_euclidean_neg_flip","right_euclidean"]),
  
        ("right_euclidean_flip",["total_neg_flip","coreflexive_neg_flip"]),
  
        ("right_euclidean_flip",["antisymmetric_flip","reflexive_neg_flip","right_euclidean_neg"]),
  
        ("right_euclidean_flip",["antisymmetric_flip","serial_neg_flip","right_euclidean_neg"]),
  
        ("right_euclidean_flip",["serial_flip","antisymmetric_flip","right_euclidean_neg"]),
  
        ("right_euclidean_flip",["reflexive_flip","symmetric_flip","transitive_neg_flip"]),
  
        ("right_euclidean_flip",["serial_flip","right_euclidean"]),
  
        ("right_euclidean_flip",["symmetric_flip","right_euclidean"]),
  
        ("right_euclidean_flip",["serial_neg_flip","antisymmetric_neg_flip","right_euclidean"]),
  
        ("right_euclidean_flip",["coreflexive_neg_flip","right_euclidean"]),
  
        ("coreflexive_flip",["antisymmetric_flip","coreflexive_neg_flip"]),
  
        ("coreflexive_flip",["symmetric_flip","total_neg_flip"]),
  
        ("coreflexive_flip",["transitive_flip","reflexive_neg_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_flip",["reflexive_flip","antisymmetric_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_flip",["symmetric_flip","antisymmetric_flip"]),
  
        ("coreflexive_flip",["reflexive_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive_flip",["reflexive_neg_flip","right_euclidean"]),
  
        ("coreflexive_flip",["total_flip","antisymmetric_flip","right_euclidean_flip"]),
  
        ("coreflexive_flip",["reflexive_flip","antisymmetric_flip","right_euclidean_flip"]),
  
        ("coreflexive_flip",["reflexive_flip","antisymmetric_flip","right_euclidean"]),
  
        ("coreflexive_flip",["transitive_neg_flip","right_euclidean_flip","serial_neg"]),
  
        ("coreflexive_flip",["total_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive_flip",["total_flip","antisymmetric_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_flip",["total_flip","antisymmetric_flip","right_euclidean"]),
  
        ("coreflexive_flip",["reflexive_flip","antisymmetric_flip","right_euclidean_neg"]),
  
        ("coreflexive_flip",["serial_neg_flip","coreflexive_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive_flip",["serial_neg_flip","coreflexive_neg_flip","right_euclidean"]),
  
        ("coreflexive_flip",["coreflexive_neg_flip","right_euclidean_flip","serial_neg"]),
  
        ("coreflexive_flip",["transitive_flip","serial_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive_flip",["coreflexive_neg_flip","serial_neg","right_euclidean"]),
  
        ("coreflexive_flip",["total_neg_flip","coreflexive_neg_flip"]),
  
        ("coreflexive_flip",["antisymmetric_flip","right_euclidean_flip","right_euclidean_neg"]),
  
        ("coreflexive_flip",["antisymmetric_flip","serial_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive_flip",["antisymmetric_flip","serial","right_euclidean_flip"]),
  
        ("coreflexive_flip",["total_neg_flip","right_euclidean"]),
  
        ("coreflexive_flip",["total_flip","antisymmetric_flip","right_euclidean_neg"]),
  
        ("coreflexive_flip",["antisymmetric_flip","right_euclidean_neg_flip","right_euclidean"]),
  
        ("coreflexive_flip",["serial_neg_flip","right_euclidean","right_euclidean_neg"]),
  
        ("coreflexive_flip",["symmetric_flip","transitive_flip","serial_neg","right_euclidean_neg"]),
  
        ("coreflexive_flip",["symmetric_flip","antisymmetric_neg_flip","serial_neg","right_euclidean"]),
  
        ("coreflexive_flip",["transitive_flip","serial_neg_flip","coreflexive_neg_flip"]),
  
        ("coreflexive_flip",["serial_neg_flip","transitive_neg_flip","right_euclidean"]),
  
        ("coreflexive_flip",["antisymmetric_flip","reflexive_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive_flip",["symmetric_flip","transitive_flip","reflexive_neg_flip"]),
  
        ("coreflexive_flip",["antisymmetric_flip","right_euclidean_flip","right_euclidean"]),
  
        ("coreflexive_flip",["symmetric_flip","transitive_flip","serial_neg_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_flip",["transitive_flip","reflexive_neg_flip","coreflexive_neg_flip"]),
  
        ("coreflexive_flip",["transitive_flip","reflexive_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive_flip",["antisymmetric_flip","reflexive_neg_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_flip",["total_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive_flip",["total_neg_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_flip",["antisymmetric_flip","right_euclidean_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive_flip",["serial_flip","antisymmetric_flip","right_euclidean_neg"]),
  
        ("coreflexive_flip",["serial_flip","antisymmetric_flip","right_euclidean"]),
  
        ("coreflexive_flip",["serial_neg","right_euclidean_neg_flip","right_euclidean"]),
  
        ("coreflexive_flip",["transitive_flip","coreflexive_neg_flip","serial_neg"]),
  
        ("coreflexive_flip",["symmetric_flip","transitive_flip","serial_neg_flip","antisymmetric_neg_flip"]),
  
        ("coreflexive_flip",["serial_neg_flip","right_euclidean_flip","right_euclidean_neg"]),
  
        ("coreflexive_flip",["right_euclidean_flip","serial_neg","right_euclidean_neg"]),
  
        ("coreflexive_flip",["right_euclidean_flip","antisymmetric_neg_flip","serial_neg"]),
  
        ("coreflexive_flip",["symmetric_flip","serial_neg_flip","transitive_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive_flip",["symmetric_flip","serial_neg_flip","right_euclidean_flip","antisymmetric_neg_flip"]),
  
        ("coreflexive_flip",["symmetric_flip","serial_neg_flip","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_flip",["transitive_flip","serial_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive_flip",["symmetric_flip","transitive_flip","serial_neg_flip","transitive_neg_flip"]),
  
        ("coreflexive_flip",["symmetric_flip","transitive_flip","antisymmetric_neg_flip","serial_neg"]),
  
        ("coreflexive_flip",["symmetric_flip","serial_neg","right_euclidean","right_euclidean_neg"]),
  
        ("coreflexive_flip",["symmetric_flip","transitive_flip","transitive_neg_flip","serial_neg"]),
  
        ("coreflexive_flip",["antisymmetric_flip","serial_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive_flip",["antisymmetric_flip","serial","right_euclidean_neg_flip"]),
  
        ("coreflexive_flip",["symmetric_flip","transitive_neg_flip","serial_neg","right_euclidean"]),
  
        ("coreflexive_flip",["serial_neg_flip","antisymmetric_neg_flip","right_euclidean"]),
  
        ("coreflexive_flip",["right_euclidean_flip","serial_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive_flip",["serial_neg_flip","right_euclidean_neg_flip","right_euclidean"]),
  
        ("antisymmetric_flip",["total_neg_flip"]),
  
        ("antisymmetric_flip",["coreflexive_flip"]),
  
        ("antisymmetric_flip",["reflexive_neg_flip","right_euclidean_flip"]),
  
        ("antisymmetric_flip",["coreflexive_neg_flip","serial_neg","right_euclidean"]),
  
        ("antisymmetric_flip",["symmetric_flip","transitive_flip","serial_neg_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric_flip",["transitive_flip","serial_neg","right_euclidean_neg_flip"]),
  
        ("antisymmetric_flip",["transitive_flip","reflexive_neg_flip"]),
  
        ("antisymmetric_flip",["reflexive_neg_flip","right_euclidean"]),
  
        ("antisymmetric_flip",["serial_neg_flip","coreflexive_neg_flip","right_euclidean_flip"]),
  
        ("antisymmetric_flip",["serial_neg_flip","coreflexive_neg_flip","right_euclidean"]),
  
        ("antisymmetric_flip",["transitive_neg_flip","right_euclidean_flip","serial_neg"]),
  
        ("antisymmetric_flip",["transitive_flip","serial_neg_flip","coreflexive_neg_flip"]),
  
        ("antisymmetric_flip",["symmetric_flip","transitive_flip","serial_neg_flip","antisymmetric_neg_flip"]),
  
        ("antisymmetric_flip",["coreflexive_neg_flip","right_euclidean_flip","serial_neg"]),
  
        ("antisymmetric_flip",["transitive_flip","coreflexive_neg_flip","serial_neg"]),
  
        ("antisymmetric_flip",["symmetric_flip","serial_neg_flip","right_euclidean_flip","antisymmetric_neg_flip"]),
  
        ("antisymmetric_flip",["symmetric_flip","serial_neg_flip","transitive_neg_flip","right_euclidean_flip"]),
  
        ("antisymmetric_flip",["serial_neg_flip","right_euclidean_flip","right_euclidean_neg"]),
  
        ("antisymmetric_flip",["transitive_flip","serial_neg_flip","right_euclidean_neg"]),
  
        ("antisymmetric_flip",["serial_neg","right_euclidean_neg_flip","right_euclidean"]),
  
        ("antisymmetric_flip",["symmetric_flip","transitive_flip","serial_neg","right_euclidean_neg"]),
  
        ("antisymmetric_flip",["symmetric_flip","serial_neg","right_euclidean","right_euclidean_neg"]),
  
        ("antisymmetric_flip",["serial_neg_flip","right_euclidean","right_euclidean_neg"]),
  
        ("antisymmetric_flip",["right_euclidean_flip","antisymmetric_neg_flip","serial_neg"]),
  
        ("antisymmetric_flip",["symmetric_flip","transitive_flip","antisymmetric_neg_flip","serial_neg"]),
  
        ("antisymmetric_flip",["symmetric_flip","antisymmetric_neg_flip","serial_neg","right_euclidean"]),
  
        ("antisymmetric_flip",["right_euclidean_flip","serial_neg","right_euclidean_neg"]),
  
        ("antisymmetric_flip",["symmetric_flip","transitive_flip","transitive_neg_flip","serial_neg"]),
  
        ("antisymmetric_flip",["symmetric_flip","transitive_flip","serial_neg_flip","transitive_neg_flip"]),
  
        ("antisymmetric_flip",["serial_neg_flip","right_euclidean_neg_flip","right_euclidean"]),
  
        ("antisymmetric_flip",["symmetric_flip","serial_neg_flip","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric_flip",["right_euclidean_flip","serial_neg","right_euclidean_neg_flip"]),
  
        ("antisymmetric_flip",["symmetric_flip","transitive_neg_flip","serial_neg","right_euclidean"]),
  
        ("antisymmetric_flip",["serial_neg_flip","antisymmetric_neg_flip","right_euclidean"]),
  
        ("antisymmetric_flip",["serial_neg_flip","transitive_neg_flip","right_euclidean"]),
  
        ("total_flip",["serial_flip","coreflexive_flip","coreflexive_neg_flip"]),
  
        ("total_flip",["serial_flip","antisymmetric_flip","coreflexive_neg_flip"]),
  
        ("total_flip",["coreflexive_flip","serial","right_euclidean_neg"]),
  
        ("total_flip",["reflexive_flip","coreflexive_neg_flip"]),
  
        ("total_flip",["coreflexive_flip","serial","coreflexive_neg_flip"]),
  
        ("total_flip",["serial_flip","coreflexive_flip","antisymmetric_neg_flip"]),
  
        ("total_flip",["serial_flip","symmetric_flip","antisymmetric_flip","antisymmetric_neg_flip"]),
  
        ("total_flip",["antisymmetric_flip","serial","coreflexive_neg_flip"]),
  
        ("total_flip",["reflexive_flip","antisymmetric_neg_flip"]),
  
        ("total_flip",["reflexive_flip","right_euclidean_neg_flip"]),
  
        ("total_flip",["serial","coreflexive_neg_flip","right_euclidean_flip"]),
  
        ("total_flip",["symmetric_flip","antisymmetric_flip","serial","right_euclidean_neg"]),
  
        ("total_flip",["transitive_flip","serial","right_euclidean_neg_flip"]),
  
        ("total_flip",["reflexive_flip","right_euclidean_neg"]),
  
        ("total_flip",["reflexive_flip","transitive_neg_flip"]),
  
        ("total_flip",["serial_flip","coreflexive_flip","right_euclidean_neg_flip"]),
  
        ("total_flip",["serial_flip","coreflexive_flip","right_euclidean_neg"]),
  
        ("total_flip",["coreflexive_flip","serial","antisymmetric_neg_flip"]),
  
        ("total_flip",["serial","transitive_neg_flip","right_euclidean_flip"]),
  
        ("total_flip",["serial_flip","coreflexive_neg_flip","right_euclidean"]),
  
        ("total_flip",["serial_flip","antisymmetric_neg_flip","right_euclidean"]),
  
        ("total_flip",["symmetric_flip","antisymmetric_flip","serial","antisymmetric_neg_flip"]),
  
        ("total_flip",["symmetric_flip","transitive_flip","serial","antisymmetric_neg_flip"]),
  
        ("total_flip",["serial_flip","symmetric_flip","antisymmetric_flip","transitive_neg_flip"]),
  
        ("total_flip",["symmetric_flip","serial","antisymmetric_neg_flip","right_euclidean"]),
  
        ("total_flip",["serial_flip","symmetric_flip","transitive_flip","right_euclidean_neg_flip"]),
  
        ("total_flip",["serial_flip","symmetric_flip","transitive_neg_flip","right_euclidean_flip"]),
  
        ("total_flip",["serial","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("total_flip",["transitive_flip","serial","coreflexive_neg_flip"]),
  
        ("total_flip",["serial_flip","right_euclidean","right_euclidean_neg"]),
  
        ("total_flip",["symmetric_flip","transitive_flip","serial","transitive_neg_flip"]),
  
        ("total_flip",["serial_flip","coreflexive_flip","transitive_neg_flip"]),
  
        ("total_flip",["coreflexive_flip","serial","transitive_neg_flip"]),
  
        ("total_flip",["serial_flip","symmetric_flip","antisymmetric_flip","right_euclidean_neg_flip"]),
  
        ("total_flip",["serial","right_euclidean_flip","right_euclidean_neg"]),
  
        ("total_flip",["antisymmetric_flip","serial","right_euclidean_neg_flip"]),
  
        ("total_flip",["serial_flip","symmetric_flip","right_euclidean_flip","antisymmetric_neg_flip"]),
  
        ("total_flip",["serial_flip","symmetric_flip","transitive_flip","antisymmetric_neg_flip"]),
  
        ("total_flip",["serial_flip","transitive_flip","coreflexive_neg_flip"]),
  
        ("total_flip",["serial","right_euclidean_flip","antisymmetric_neg_flip"]),
  
        ("total_flip",["serial_flip","antisymmetric_flip","right_euclidean_neg"]),
  
        ("total_flip",["serial_flip","transitive_neg_flip","right_euclidean"]),
  
        ("total_flip",["serial_flip","right_euclidean_neg_flip","right_euclidean"]),
  
        ("total_flip",["serial","right_euclidean_neg_flip","right_euclidean"]),
  
        ("total_flip",["symmetric_flip","antisymmetric_flip","serial","transitive_neg_flip"]),
  
        ("total_flip",["symmetric_flip","serial","right_euclidean","right_euclidean_neg"]),
  
        ("total_flip",["serial_flip","symmetric_flip","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("total_flip",["serial_flip","transitive_flip","right_euclidean_neg"]),
  
        ("total_flip",["coreflexive_flip","serial","right_euclidean_neg_flip"]),
  
        ("total_flip",["symmetric_flip","transitive_flip","serial","right_euclidean_neg"]),
  
        ("total_flip",["serial_flip","symmetric_flip","transitive_flip","transitive_neg_flip"]),
  
        ("total_flip",["serial_flip","coreflexive_neg_flip","right_euclidean_flip"]),
  
        ("total_flip",["serial","coreflexive_neg_flip","right_euclidean"]),
  
        ("total_flip",["symmetric_flip","serial","transitive_neg_flip","right_euclidean"]),
  
        ("total_flip",["serial_flip","right_euclidean_flip","right_euclidean_neg"]),
  
        ("serial_flip",["reflexive_flip"]),
  
        ("serial_flip",["total_flip"]),
  
        ("serial_flip",["serial","coreflexive_neg_flip"]),
  
        ("serial_flip",["coreflexive_flip","serial"]),
  
        ("serial_flip",["symmetric_flip","serial"]),
  
        ("serial_flip",["serial","right_euclidean_flip"]),
  
        ("serial_flip",["serial","right_euclidean_neg_flip"]),
  
        ("serial_flip",["serial","reflexive_neg_flip","right_euclidean_neg"]),
  
        ("serial_flip",["serial_neg_flip","serial","right_euclidean_neg"]),
	
        ("transitive_neg_flip",["total_neg_flip","symmetric_neg_flip"]),
  
        ("transitive_neg_flip",["symmetric_neg_flip","total_flip"]),
  
        ("transitive_neg_flip",["antisymmetric_neg_flip","coreflexive_flip"]),
  
        ("transitive_neg_flip",["coreflexive_neg_flip"]),
  
        ("transitive_neg_flip",["transitive_flip","right_euclidean_neg_flip"]),
  
        ("transitive_neg_flip",["reflexive_neg_flip","right_euclidean_flip"]),
  
        ("transitive_neg_flip",["reflexive_flip","right_euclidean_neg_flip"]),
  
        ("transitive_neg_flip",["antisymmetric_neg_flip","transitive_flip"]),
  
        ("transitive_neg_flip",["total_neg_flip","right_euclidean_flip"]),
  
        ("transitive_neg_flip",["reflexive_neg_flip","coreflexive_flip"]),
  
        ("transitive_neg_flip",["total_flip","coreflexive_flip"]),
  
        ("transitive_neg_flip",["total_flip","right_euclidean_neg_flip"]),
  
        ("transitive_neg_flip",["symmetric_neg_flip","antisymmetric_neg_flip"]),
  
        ("transitive_neg_flip",["symmetric_neg_flip","right_euclidean_neg_flip"]),
  
        ("transitive_neg_flip",["reflexive_neg_flip","symmetric_neg_flip","antisymmetric_flip"]),
  
        ("transitive_neg_flip",["right_euclidean_neg_flip","right_euclidean_flip"]),
  
        ("transitive_neg_flip",["total_neg_flip","right_euclidean_neg_flip"]),
  
        ("transitive_neg_flip",["total_neg_flip","coreflexive_flip"]),
  
        ("transitive_neg_flip",["total_flip","right_euclidean_flip"]),
  
        ("transitive_neg_flip",["total_flip","transitive_flip"]),
  
        ("transitive_neg_flip",["reflexive_neg_flip","right_euclidean"]),
  
        ("transitive_neg_flip",["symmetric_neg_flip","right_euclidean_neg"]),
  
        ("transitive_neg_flip",["reflexive_neg_flip","right_euclidean_neg_flip"]),
  
        ("transitive_neg_flip",["antisymmetric_neg_flip","right_euclidean_flip"]),
  
        ("transitive_neg_flip",["antisymmetric_neg_flip","right_euclidean_neg_flip"]),
  
        ("transitive_neg_flip",["total_flip","right_euclidean_neg"]),
  
        ("transitive_neg_flip",["coreflexive_flip","right_euclidean_neg"]),
  
        ("transitive_neg_flip",["reflexive_neg_flip","right_euclidean_neg"]),
  
        ("transitive_neg_flip",["total_neg_flip","right_euclidean_neg"]),
  
        ("transitive_neg_flip",["serial_neg","right_euclidean_neg_flip"]),
  
        ("transitive_neg_flip",["total_neg_flip","right_euclidean"]),
  
        ("transitive_neg_flip",["total_flip","right_euclidean"]),
  
        ("transitive_neg_flip",["coreflexive_flip","right_euclidean_neg_flip"]),
  
        ("transitive_neg_flip",["right_euclidean_neg_flip","antisymmetric_flip"]),
  
        ("transitive_neg_flip",["right_euclidean_neg_flip","right_euclidean_neg"]),
  
        ("transitive_neg_flip",["reflexive_neg_flip","symmetric_neg_flip","transitive_flip"]),
  
        ("transitive_neg_flip",["antisymmetric_neg_flip","right_euclidean"]),
  
        ("transitive_neg_flip",["transitive_flip","right_euclidean_neg"]),
  
        ("transitive_neg_flip",["antisymmetric_neg_flip","right_euclidean_neg"]),
  
        ("transitive_neg_flip",["serial_neg_flip","right_euclidean_neg"]),
  
        ("transitive_neg_flip",["antisymmetric_flip","right_euclidean_neg"]),
  
        ("transitive_neg_flip",["right_euclidean_neg","right_euclidean"]),
  
        ("transitive_neg_flip",["reflexive_flip","right_euclidean_neg"]),
  
        ("transitive_neg_flip",["right_euclidean_neg_flip","right_euclidean"]),
  
        ("transitive_neg_flip",["right_euclidean_flip","right_euclidean_neg"]),
  
        ("reflexive_neg_flip",["serial_neg_flip","antisymmetric_neg_flip","coreflexive_flip"]),
  
        ("reflexive_neg_flip",["antisymmetric_neg_flip","serial_neg","coreflexive_flip"]),
  
        ("reflexive_neg_flip",["total_neg_flip"]),
  
        ("reflexive_neg_flip",["transitive_neg_flip","serial_neg","right_euclidean_flip"]),
  
        ("reflexive_neg_flip",["serial_neg_flip","coreflexive_neg_flip"]),
  
        ("reflexive_neg_flip",["serial_neg","right_euclidean_neg_flip"]),
  
        ("reflexive_neg_flip",["symmetric_neg_flip","transitive_neg_flip","serial_neg"]),
  
        ("reflexive_neg_flip",["symmetric_neg_flip","serial_neg","right_euclidean_neg"]),
  
        ("reflexive_neg_flip",["serial_neg_flip","symmetric_neg_flip","right_euclidean_neg_flip"]),
  
        ("reflexive_neg_flip",["serial_neg_flip","right_euclidean_neg"]),
  
        ("reflexive_neg_flip",["serial_neg_flip","symmetric_neg_flip","transitive_neg_flip"]),
  
        ("reflexive_neg_flip",["transitive_neg_flip","serial_neg","coreflexive_flip"]),
  
        ("reflexive_neg_flip",["coreflexive_neg_flip","serial_neg"]),
  
        ("reflexive_neg_flip",["serial_neg_flip","symmetric_neg_flip","antisymmetric_neg_flip"]),
  
        ("reflexive_neg_flip",["serial_neg_flip","transitive_neg_flip","right_euclidean"]),
  
        ("reflexive_neg_flip",["serial_neg_flip","antisymmetric_neg_flip","right_euclidean"]),
  
        ("reflexive_neg_flip",["symmetric_neg_flip","antisymmetric_neg_flip","serial_neg"]),
  
        ("reflexive_neg_flip",["serial_neg","right_euclidean_flip","right_euclidean_neg"]),
  
        ("reflexive_neg_flip",["antisymmetric_neg_flip","serial_neg","right_euclidean_flip"]),
  
        ("reflexive_neg_flip",["serial_neg_flip","transitive_neg_flip","coreflexive_flip"]),
  
        ("reflexive_neg_flip",["serial_neg_flip","right_euclidean_neg_flip","right_euclidean"]),
  
        ("reflexive_neg_flip",["serial_neg_flip","coreflexive_flip","right_euclidean_neg_flip"]),
  
        ("reflexive_neg_flip",["serial_neg","coreflexive_flip","right_euclidean_neg"]),
  
        ("symmetric_neg_flip",["transitive_neg_flip","serial_neg","right_euclidean_flip"]),
  
        ("symmetric_neg_flip",["coreflexive_neg_flip"]),
  
        ("symmetric_neg_flip",["reflexive_flip","right_euclidean_neg_flip"]),
  
        ("symmetric_neg_flip",["reflexive_flip","right_euclidean_neg"]),
  
        ("symmetric_neg_flip",["coreflexive_flip"]),
  
        ("symmetric_neg_flip",["reflexive_neg_flip","right_euclidean_flip"]),
  
        ("symmetric_neg_flip",["reflexive_neg_flip","right_euclidean"]),
  
        ("symmetric_neg_flip",["reflexive_neg_flip","right_euclidean_neg_flip"]),
  
        ("symmetric_neg_flip",["total_flip","right_euclidean_neg_flip"]),
  
        ("symmetric_neg_flip",["transitive_flip","right_euclidean_neg_flip","serial"]),
  
        ("symmetric_neg_flip",["reflexive_flip","right_euclidean_flip"]),
  
        ("symmetric_neg_flip",["total_flip","right_euclidean_flip"]),
  
        ("symmetric_neg_flip",["total_neg_flip","right_euclidean_flip"]),
  
        ("symmetric_neg_flip",["total_neg_flip","right_euclidean_neg_flip"]),
  
        ("symmetric_neg_flip",["total_neg_flip","right_euclidean_neg"]),
  
        ("symmetric_neg_flip",["total_neg_flip","right_euclidean"]),
  
        ("symmetric_neg_flip",["serial_neg","right_euclidean_neg_flip"]),
  
        ("symmetric_neg_flip",["total_flip","right_euclidean_neg"]),
  
        ("symmetric_neg_flip",["reflexive_flip","right_euclidean"]),
  
        ("symmetric_neg_flip",["right_euclidean_neg_flip","right_euclidean"]),
  
        ("symmetric_neg_flip",["reflexive_neg_flip","right_euclidean_neg"]),
  
        ("symmetric_neg_flip",["right_euclidean_flip","right_euclidean_neg"]),
  
        ("symmetric_neg_flip",["right_euclidean_neg_flip","right_euclidean_neg"]),
  
        ("symmetric_neg_flip",["serial_flip","transitive_flip","right_euclidean_neg"]),
  
        ("symmetric_neg_flip",["serial_flip","right_euclidean"]),
  
        ("symmetric_neg_flip",["right_euclidean_flip","right_euclidean"]),
  
        ("symmetric_neg_flip",["serial_neg_flip","transitive_neg_flip","right_euclidean"]),
  
        ("symmetric_neg_flip",["antisymmetric_neg_flip","serial_neg","right_euclidean_flip"]),
  
        ("symmetric_neg_flip",["serial","right_euclidean_flip"]),
  
        ("symmetric_neg_flip",["serial_neg_flip","right_euclidean_neg"]),
  
        ("symmetric_neg_flip",["serial_neg_flip","antisymmetric_neg_flip","right_euclidean"]),
  
        ("symmetric_neg_flip",["total_flip","right_euclidean"]),
  
        ("symmetric_neg_flip",["right_euclidean_neg_flip","antisymmetric_flip","serial"]),
  
        ("symmetric_neg_flip",["serial_flip","antisymmetric_flip","right_euclidean_neg"]),
  
        ("right_euclidean_neg_flip",["symmetric_neg_flip","total_flip"]),
  
        ("right_euclidean_neg_flip",["total_neg_flip","symmetric_neg_flip"]),
  
        ("right_euclidean_neg_flip",["antisymmetric_neg_flip","coreflexive_flip"]),
  
        ("right_euclidean_neg_flip",["transitive_neg_flip","right_euclidean_flip"]),
  
        ("right_euclidean_neg_flip",["reflexive_neg_flip","right_euclidean_flip"]),
  
        ("right_euclidean_neg_flip",["reflexive_neg_flip","coreflexive_flip"]),
  
        ("right_euclidean_neg_flip",["total_flip","right_euclidean_flip"]),
  
        ("right_euclidean_neg_flip",["coreflexive_neg_flip"]),
  
        ("right_euclidean_neg_flip",["reflexive_flip","right_euclidean_neg"]),
  
        ("right_euclidean_neg_flip",["total_neg_flip","coreflexive_flip"]),
  
        ("right_euclidean_neg_flip",["total_neg_flip","right_euclidean_flip"]),
  
        ("right_euclidean_neg_flip",["transitive_neg_flip","reflexive_flip","right_euclidean"]),
  
        ("right_euclidean_neg_flip",["reflexive_neg_flip","right_euclidean"]),
  
        ("right_euclidean_neg_flip",["transitive_neg_flip","coreflexive_flip"]),
  
        ("right_euclidean_neg_flip",["total_flip","right_euclidean_neg"]),
  
        ("right_euclidean_neg_flip",["serial_flip","right_euclidean_neg","right_euclidean"]),
  
        ("right_euclidean_neg_flip",["symmetric_neg_flip","antisymmetric_neg_flip"]),
  
        ("right_euclidean_neg_flip",["total_neg_flip","right_euclidean_neg"]),
  
        ("right_euclidean_neg_flip",["serial_flip","transitive_flip","right_euclidean_neg"]),
  
        ("right_euclidean_neg_flip",["transitive_neg_flip","serial_flip","right_euclidean"]),
  
        ("right_euclidean_neg_flip",["total_flip","right_euclidean"]),
  
        ("right_euclidean_neg_flip",["antisymmetric_neg_flip","right_euclidean_flip"]),
  
        ("right_euclidean_neg_flip",["symmetric_neg_flip","transitive_neg_flip"]),
  
        ("right_euclidean_neg_flip",["total_neg_flip","right_euclidean"]),
  
        ("right_euclidean_neg_flip",["reflexive_neg_flip","symmetric_neg_flip","antisymmetric_flip"]),
  
        ("right_euclidean_neg_flip",["serial_neg_flip","transitive_neg_flip","right_euclidean"]),
  
        ("right_euclidean_neg_flip",["reflexive_neg_flip","right_euclidean_neg"]),
  
        ("right_euclidean_neg_flip",["right_euclidean_flip","right_euclidean_neg"]),
  
        ("right_euclidean_neg_flip",["total_flip","coreflexive_flip"]),
  
        ("right_euclidean_neg_flip",["antisymmetric_neg_flip","reflexive_flip","right_euclidean"]),
  
        ("right_euclidean_neg_flip",["antisymmetric_neg_flip","serial_flip","right_euclidean"]),
  
        ("right_euclidean_neg_flip",["serial_neg_flip","antisymmetric_neg_flip","right_euclidean"]),
  
        ("right_euclidean_neg_flip",["reflexive_neg_flip","symmetric_neg_flip","transitive_flip"]),
  
        ("right_euclidean_neg_flip",["serial_neg_flip","right_euclidean_neg"]),
  
        ("right_euclidean_neg_flip",["symmetric_neg_flip","right_euclidean_neg"]),
  
        ("right_euclidean_neg_flip",["serial_flip","antisymmetric_flip","right_euclidean_neg"]),
  
        ("right_euclidean_neg_flip",["coreflexive_flip","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["antisymmetric_neg_flip","coreflexive_flip"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","total_flip"]),
  
        ("coreflexive_neg_flip",["transitive_neg_flip","reflexive_flip","right_euclidean_flip"]),
  
        ("coreflexive_neg_flip",["reflexive_neg_flip","antisymmetric_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","antisymmetric_neg_flip"]),
  
        ("coreflexive_neg_flip",["reflexive_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg_flip",["reflexive_flip","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["total_neg_flip","antisymmetric_neg_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg_flip",["reflexive_neg_flip","antisymmetric_neg_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg_flip",["reflexive_neg_flip","antisymmetric_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["transitive_flip","right_euclidean_neg_flip","serial"]),
  
        ("coreflexive_neg_flip",["total_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg_flip",["total_neg_flip","antisymmetric_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive_neg_flip",["total_neg_flip","antisymmetric_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["reflexive_neg_flip","antisymmetric_neg_flip","right_euclidean"]),
  
        ("coreflexive_neg_flip",["serial_flip","coreflexive_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg_flip",["serial_flip","coreflexive_flip","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["coreflexive_flip","right_euclidean_neg_flip","serial"]),
  
        ("coreflexive_neg_flip",["transitive_neg_flip","serial","right_euclidean_flip"]),
  
        ("coreflexive_neg_flip",["coreflexive_flip","serial","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["total_flip","coreflexive_flip"]),
  
        ("coreflexive_neg_flip",["antisymmetric_neg_flip","right_euclidean_neg_flip","right_euclidean"]),
  
        ("coreflexive_neg_flip",["antisymmetric_neg_flip","serial","right_euclidean_flip"]),
  
        ("coreflexive_neg_flip",["antisymmetric_neg_flip","serial_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg_flip",["total_flip","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["total_neg_flip","antisymmetric_neg_flip","right_euclidean"]),
  
        ("coreflexive_neg_flip",["antisymmetric_neg_flip","right_euclidean_flip","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["serial_flip","right_euclidean_neg","right_euclidean"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","transitive_neg_flip","serial","right_euclidean"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","antisymmetric_flip","serial","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["transitive_neg_flip","serial_flip","coreflexive_flip"]),
  
        ("coreflexive_neg_flip",["serial_flip","transitive_flip","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["antisymmetric_neg_flip","reflexive_flip","right_euclidean"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","transitive_neg_flip","reflexive_flip"]),
  
        ("coreflexive_neg_flip",["antisymmetric_neg_flip","right_euclidean_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","transitive_neg_flip","serial_flip","right_euclidean_flip"]),
  
        ("coreflexive_neg_flip",["transitive_neg_flip","reflexive_flip","coreflexive_flip"]),
  
        ("coreflexive_neg_flip",["transitive_neg_flip","reflexive_flip","right_euclidean"]),
  
        ("coreflexive_neg_flip",["antisymmetric_neg_flip","reflexive_flip","right_euclidean_flip"]),
  
        ("coreflexive_neg_flip",["total_flip","right_euclidean"]),
  
        ("coreflexive_neg_flip",["total_flip","right_euclidean_flip"]),
  
        ("coreflexive_neg_flip",["antisymmetric_neg_flip","right_euclidean_flip","right_euclidean"]),
  
        ("coreflexive_neg_flip",["serial_neg_flip","antisymmetric_neg_flip","right_euclidean"]),
  
        ("coreflexive_neg_flip",["serial_neg_flip","antisymmetric_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["serial","right_euclidean_flip","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["transitive_neg_flip","coreflexive_flip","serial"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","transitive_neg_flip","serial_flip","antisymmetric_flip"]),
  
        ("coreflexive_neg_flip",["serial_flip","right_euclidean_neg_flip","right_euclidean"]),
  
        ("coreflexive_neg_flip",["right_euclidean_neg_flip","serial","right_euclidean"]),
  
        ("coreflexive_neg_flip",["right_euclidean_neg_flip","antisymmetric_flip","serial"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","serial_flip","transitive_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","serial_flip","right_euclidean_neg_flip","antisymmetric_flip"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","serial_flip","right_euclidean_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive_neg_flip",["transitive_neg_flip","serial_flip","right_euclidean"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","transitive_neg_flip","serial_flip","transitive_flip"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","transitive_neg_flip","antisymmetric_flip","serial"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","serial","right_euclidean_neg","right_euclidean"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","transitive_neg_flip","transitive_flip","serial"]),
  
        ("coreflexive_neg_flip",["antisymmetric_neg_flip","serial_flip","right_euclidean"]),
  
        ("coreflexive_neg_flip",["antisymmetric_neg_flip","serial_neg","right_euclidean_flip"]),
  
        ("coreflexive_neg_flip",["symmetric_neg_flip","transitive_flip","serial","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["serial_flip","antisymmetric_flip","right_euclidean_neg"]),
  
        ("coreflexive_neg_flip",["right_euclidean_neg_flip","serial","right_euclidean_flip"]),
  
        ("coreflexive_neg_flip",["serial_flip","right_euclidean_flip","right_euclidean_neg"]),
  
        ("antisymmetric_neg_flip",["total_flip"]),
  
        ("antisymmetric_neg_flip",["coreflexive_neg_flip"]),
  
        ("antisymmetric_neg_flip",["reflexive_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric_neg_flip",["coreflexive_flip","serial","right_euclidean_neg"]),
  
        ("antisymmetric_neg_flip",["symmetric_neg_flip","transitive_neg_flip","serial_flip","right_euclidean_flip"]),
  
        ("antisymmetric_neg_flip",["transitive_neg_flip","serial","right_euclidean_flip"]),
  
        ("antisymmetric_neg_flip",["transitive_neg_flip","reflexive_flip"]),
  
        ("antisymmetric_neg_flip",["reflexive_flip","right_euclidean_neg"]),
  
        ("antisymmetric_neg_flip",["serial_flip","coreflexive_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric_neg_flip",["serial_flip","coreflexive_flip","right_euclidean_neg"]),
  
        ("antisymmetric_neg_flip",["transitive_flip","right_euclidean_neg_flip","serial"]),
  
        ("antisymmetric_neg_flip",["transitive_neg_flip","serial_flip","coreflexive_flip"]),
  
        ("antisymmetric_neg_flip",["symmetric_neg_flip","transitive_neg_flip","serial_flip","antisymmetric_flip"]),
  
        ("antisymmetric_neg_flip",["coreflexive_flip","right_euclidean_neg_flip","serial"]),
  
        ("antisymmetric_neg_flip",["transitive_neg_flip","coreflexive_flip","serial"]),
  
        ("antisymmetric_neg_flip",["symmetric_neg_flip","serial_flip","right_euclidean_neg_flip","antisymmetric_flip"]),
  
        ("antisymmetric_neg_flip",["symmetric_neg_flip","serial_flip","transitive_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric_neg_flip",["serial_flip","right_euclidean_neg_flip","right_euclidean"]),
  
        ("antisymmetric_neg_flip",["transitive_neg_flip","serial_flip","right_euclidean"]),
  
        ("antisymmetric_neg_flip",["serial","right_euclidean_flip","right_euclidean_neg"]),
  
        ("antisymmetric_neg_flip",["symmetric_neg_flip","transitive_neg_flip","serial","right_euclidean"]),
  
        ("antisymmetric_neg_flip",["symmetric_neg_flip","serial","right_euclidean_neg","right_euclidean"]),
  
        ("antisymmetric_neg_flip",["serial_flip","right_euclidean_neg","right_euclidean"]),
  
        ("antisymmetric_neg_flip",["right_euclidean_neg_flip","antisymmetric_flip","serial"]),
  
        ("antisymmetric_neg_flip",["symmetric_neg_flip","transitive_neg_flip","antisymmetric_flip","serial"]),
  
        ("antisymmetric_neg_flip",["symmetric_neg_flip","antisymmetric_flip","serial","right_euclidean_neg"]),
  
        ("antisymmetric_neg_flip",["right_euclidean_neg_flip","serial","right_euclidean"]),
  
        ("antisymmetric_neg_flip",["symmetric_neg_flip","transitive_neg_flip","transitive_flip","serial"]),
  
        ("antisymmetric_neg_flip",["symmetric_neg_flip","transitive_neg_flip","serial_flip","transitive_flip"]),
  
        ("antisymmetric_neg_flip",["serial_flip","right_euclidean_flip","right_euclidean_neg"]),
  
        ("antisymmetric_neg_flip",["symmetric_neg_flip","serial_flip","right_euclidean_neg_flip","right_euclidean_flip"]),
  
        ("antisymmetric_neg_flip",["right_euclidean_neg_flip","serial","right_euclidean_flip"]),
  
        ("antisymmetric_neg_flip",["symmetric_neg_flip","transitive_flip","serial","right_euclidean_neg"]),
  
        ("antisymmetric_neg_flip",["serial_flip","antisymmetric_flip","right_euclidean_neg"]),
  
        ("antisymmetric_neg_flip",["serial_flip","transitive_flip","right_euclidean_neg"]),
  
        ("total_neg_flip",["serial_neg_flip","coreflexive_neg_flip","coreflexive_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","antisymmetric_neg_flip","coreflexive_flip"]),
  
        ("total_neg_flip",["coreflexive_neg_flip","serial_neg","right_euclidean"]),
  
        ("total_neg_flip",["reflexive_neg_flip","coreflexive_flip"]),
  
        ("total_neg_flip",["coreflexive_neg_flip","serial_neg","coreflexive_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","coreflexive_neg_flip","antisymmetric_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","symmetric_neg_flip","antisymmetric_neg_flip","antisymmetric_flip"]),
  
        ("total_neg_flip",["antisymmetric_neg_flip","serial_neg","coreflexive_flip"]),
  
        ("total_neg_flip",["reflexive_neg_flip","antisymmetric_flip"]),
  
        ("total_neg_flip",["reflexive_neg_flip","right_euclidean_flip"]),
  
        ("total_neg_flip",["serial_neg","coreflexive_flip","right_euclidean_neg_flip"]),
  
        ("total_neg_flip",["symmetric_neg_flip","antisymmetric_neg_flip","serial_neg","right_euclidean"]),
  
        ("total_neg_flip",["transitive_neg_flip","serial_neg","right_euclidean_flip"]),
  
        ("total_neg_flip",["reflexive_neg_flip","right_euclidean"]),
  
        ("total_neg_flip",["reflexive_neg_flip","transitive_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","coreflexive_neg_flip","right_euclidean_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","coreflexive_neg_flip","right_euclidean"]),
  
        ("total_neg_flip",["coreflexive_neg_flip","serial_neg","antisymmetric_flip"]),
  
        ("total_neg_flip",["serial_neg","transitive_flip","right_euclidean_neg_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","coreflexive_flip","right_euclidean_neg"]),
  
        ("total_neg_flip",["serial_neg_flip","antisymmetric_flip","right_euclidean_neg"]),
  
        ("total_neg_flip",["symmetric_neg_flip","antisymmetric_neg_flip","serial_neg","antisymmetric_flip"]),
  
        ("total_neg_flip",["symmetric_neg_flip","transitive_neg_flip","serial_neg","antisymmetric_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","symmetric_neg_flip","antisymmetric_neg_flip","transitive_flip"]),
  
        ("total_neg_flip",["symmetric_neg_flip","serial_neg","antisymmetric_flip","right_euclidean_neg"]),
  
        ("total_neg_flip",["serial_neg_flip","symmetric_neg_flip","transitive_neg_flip","right_euclidean_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","symmetric_neg_flip","transitive_flip","right_euclidean_neg_flip"]),
  
        ("total_neg_flip",["serial_neg","right_euclidean_neg_flip","right_euclidean_flip"]),
  
        ("total_neg_flip",["transitive_neg_flip","serial_neg","coreflexive_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","right_euclidean_neg","right_euclidean"]),
  
        ("total_neg_flip",["symmetric_neg_flip","transitive_neg_flip","serial_neg","transitive_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","coreflexive_neg_flip","transitive_flip"]),
  
        ("total_neg_flip",["coreflexive_neg_flip","serial_neg","transitive_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","symmetric_neg_flip","antisymmetric_neg_flip","right_euclidean_flip"]),
  
        ("total_neg_flip",["serial_neg","right_euclidean_neg_flip","right_euclidean"]),
  
        ("total_neg_flip",["antisymmetric_neg_flip","serial_neg","right_euclidean_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","symmetric_neg_flip","right_euclidean_neg_flip","antisymmetric_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","symmetric_neg_flip","transitive_neg_flip","antisymmetric_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","transitive_neg_flip","coreflexive_flip"]),
  
        ("total_neg_flip",["serial_neg","right_euclidean_neg_flip","antisymmetric_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","antisymmetric_neg_flip","right_euclidean"]),
  
        ("total_neg_flip",["serial_neg_flip","transitive_flip","right_euclidean_neg"]),
  
        ("total_neg_flip",["serial_neg_flip","right_euclidean_flip","right_euclidean_neg"]),
  
        ("total_neg_flip",["serial_neg","right_euclidean_flip","right_euclidean_neg"]),
  
        ("total_neg_flip",["symmetric_neg_flip","antisymmetric_neg_flip","serial_neg","transitive_flip"]),
  
        ("total_neg_flip",["symmetric_neg_flip","serial_neg","right_euclidean_neg","right_euclidean"]),
  
        ("total_neg_flip",["serial_neg_flip","symmetric_neg_flip","right_euclidean_neg_flip","right_euclidean_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","transitive_neg_flip","right_euclidean"]),
  
        ("total_neg_flip",["coreflexive_neg_flip","serial_neg","right_euclidean_flip"]),
  
        ("total_neg_flip",["symmetric_neg_flip","transitive_neg_flip","serial_neg","right_euclidean"]),
  
        ("total_neg_flip",["serial_neg_flip","symmetric_neg_flip","transitive_neg_flip","transitive_flip"]),
  
        ("total_neg_flip",["serial_neg_flip","coreflexive_flip","right_euclidean_neg_flip"]),
  
        ("total_neg_flip",["serial_neg","coreflexive_flip","right_euclidean_neg"]),
  
        ("total_neg_flip",["symmetric_neg_flip","serial_neg","transitive_flip","right_euclidean_neg"]),
  
        ("total_neg_flip",["serial_neg_flip","right_euclidean_neg_flip","right_euclidean"]),
  
        ("serial_neg_flip",["reflexive_neg_flip"]),
  
        ("serial_neg_flip",["total_neg_flip"]),
  
        ("serial_neg_flip",["serial_neg","coreflexive_flip"]),
  
        ("serial_neg_flip",["coreflexive_neg_flip","serial_neg"]),
  
        ("serial_neg_flip",["symmetric_neg_flip","serial_neg"]),
  
        ("serial_neg_flip",["serial_neg","right_euclidean_neg_flip"]),
  
        ("serial_neg_flip",["serial_neg","right_euclidean_flip"]),
  
        ("serial_neg_flip",["serial_neg","reflexive_flip","right_euclidean"]),
  
        ("serial_neg_flip",["serial_flip","serial_neg","right_euclidean"])
	]
	
	