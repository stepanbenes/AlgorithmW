using System;
using System.Collections.Immutable;

namespace AlgorithmW;

public class TypeInferrer
{
    /// <summary>
    /// The meat of the type inference algorithm.
    /// </summary>
    private static TypeInferenceResult<(Substitution, InferredType)> InferType(Expression expression, TypeEnvironment typeEnvironment, TypeVarGenerator typeVarGenerator)
    {
        switch (expression)
        {
            // A variable is typed as an instantiation of the corresponding type in the
            // environment.
            case Variable(var term):
                if (typeEnvironment.TryGet(term) is Polytype polytype)
                    return TypeInferenceResult.Ok((Substitution.Empty(), InstantiateType(polytype, typeVarGenerator)));
                return TypeInferenceResult.Fail<(Substitution, InferredType)>(new TypeInferenceError($"unbound variable: {term}"));
            // A literal is typed as it's primitive type.
            case Literal literal:
                return TypeInferenceResult.Ok<(Substitution, InferredType)>((
                        Substitution.Empty(),
                        literal switch
                        {
                            IntegerLiteral(_) => new IntegerType(),
                            FloatLiteral(_) => new FloatType(),
                            BoolLiteral(_) => new BoolType(),
                            StringLiteral(_) => new StringType(),
                            _ => throw new InvalidOperationException($"unknown literal type '{literal}'"),
                        }
                    ));
            // An abstraction is typed by:
            // * Removing any existing type with the same name as the argument to prevent name clashes.
            // * Inserting a new type variable for the argument.
            // * Inferring the type of the expression in the new environment to define the type of the expression.
            // * Applying the resulting substitution to the argument to define the type of the argument.
            case AbstractionExpression({ } term, { } exp):
                {
                    var varType = new VariableType(typeVarGenerator.GetNext());
                    var env = typeEnvironment.Remove(term).Insert(term, new Polytype(new List<TypeVar>(), varType));
                    return InferType(exp, env, typeVarGenerator) switch
                    {
                        ((Substitution substitution, InferredType type), _) => TypeInferenceResult.Ok<(Substitution, InferredType)>((substitution, new FunctionType(ApplySubstitution(varType, substitution), type))),
                        (_, TypeInferenceError error) => TypeInferenceResult.Fail<(Substitution, InferredType)>(error)
                    };
                }
            // An application is typed by:
            // * Inferring the type of the callee.
            // * Applying the resulting substitution to the argument and inferring it's type.
            // * Finding the most general unifier for the callee type and a function from the
            // argument type to a new type variable. This combines the previously known type of the
            // function and the type as it is now being used.
            // * Applying the unifier to the new type variable.
            case ApplicationExpression({ } exp1, { } exp2):
                {
                    var result1 = InferType(exp1, typeEnvironment, typeVarGenerator);
                    if (result1 is ((Substitution s1, InferredType t1), _))
                    {
                        var result2 = InferType(exp2, ApplySubstitution(typeEnvironment, s1), typeVarGenerator);
                        if (result2 is ((Substitution s2, InferredType t2), _))
                        {
                            var varType = new VariableType(typeVarGenerator.GetNext());
                            var result3 = MostGeneralUnifier(type1: ApplySubstitution(t1, s2), type2: new FunctionType(t2, varType));
                            if (result3 is (Substitution s3, _))
                            {
                                return TypeInferenceResult.Ok((ComposeSubstitutions(s3, ComposeSubstitutions(s2, s1)), ApplySubstitution(varType, s3)));
                            }
                            return TypeInferenceResult.Fail<(Substitution, InferredType)>(result3.Error!);
                        }
                        return result2; // error
                    }
                    return result1; // error
                }
            // Let (variable binding) is typed by:
            // * Removing any existing type with the same name as the binding variable to prevent name clashes.
            // * Inferring the type of the binding.
            // * Applying the resulting substitution to the environment and generalizing to the binding type.
            // * Inserting the generalized type to the binding variable in the new environment.
            // * Applying the substution for the binding to the environment and inferring the type of the expression.
            case LetExpression({ } term, { } exp1, { } exp2):
                {
                    var env1 = typeEnvironment.Remove(term);
                    var result1 = InferType(exp1, typeEnvironment, typeVarGenerator);
                    if (result1 is ((Substitution s1, InferredType t1), _))
                    {
                        var tp = GeneralizePolytype(ApplySubstitution(env1, s1), t1);
                        var env2 = env1.Insert(term, tp);
                        var result2 = InferType(exp2, ApplySubstitution(env2, s1), typeVarGenerator);
                        if (result2 is ((Substitution s2, InferredType t2), _))
                        {
                            return TypeInferenceResult.Ok<(Substitution, InferredType)>((ComposeSubstitutions(s2, s1), t2));
                        }
                        return result2; // error
                    }
                    return result1; // error
                }
            default:
                throw new InvalidOperationException($"unknown expression type '{expression}'");
        }
    }

    private static InferredType ApplySubstitution(InferredType type, Substitution substitution)
    {
        return type switch
        {
            // If this type references a variable that is in the substitution, return it's
            // replacement type. Otherwise, return the existing type.
            VariableType({ } tv) => substitution.TryGet(tv) ?? type,
            // To apply to a function, we simply apply to each of the input and output.
            FunctionType({ } t1, { } t2) => new FunctionType(ApplySubstitution(t1, substitution), ApplySubstitution(t2, substitution)),
            // A primitive type is changed by a substitution.
            _ => type
        };
    }

    /// <summary>
    ///  To apply a substitution, we just apply it to each polytype in the type environment.
    /// </summary>
    private static TypeEnvironment ApplySubstitution(TypeEnvironment typeEnv, Substitution substitution)
    {
        return new TypeEnvironment(typeEnv.Enumerate().Select(kv => (kv.Key, ApplySubstitution(kv.Value, substitution))));
    }

    private static Polytype ApplySubstitution(Polytype polytype, Substitution substitution)
    {
        var sub = substitution;
        foreach(var tv in polytype.TypeVariables)
        {
            sub = sub.Remove(tv);
        }
        return new Polytype(polytype.TypeVariables, Type: ApplySubstitution(polytype.Type, sub));
    }

    private static Polytype GeneralizePolytype(TypeEnvironment typeEnv, InferredType type)
    {
        return new Polytype(TypeVariables: GetFreeTypeVariables(type).Except(GetFreeTypeVariables(typeEnv)).ToList(), type);
    }

    /// <summary>
    /// To compose two substitutions, we apply s1 to each type in s2 and union the resulting substitution with s1.
    /// </summary>
    private static Substitution ComposeSubstitutions(Substitution s1, Substitution s2)
    {
        var map = s2.Enumerate().Select(kv => (typeVar: kv.Key, type: ApplySubstitution(kv.Value, s1))).ToImmutableDictionary(x => x.typeVar, x => x.type);
        return new Substitution(map);
    }

    /// <summary>
    /// Most general unifier, a substitution S such that S(self) is congruent to S(other).
    /// </summary>
    private static TypeInferenceResult<Substitution> MostGeneralUnifier(InferredType type1, InferredType type2)
    {
        switch (type1, type2)
        {
            // For functions, we find the most general unifier for the inputs, apply the resulting
            // substitution to the outputs, find the outputs' most general unifier, and finally
            // compose the two resulting substitutions.
            case (FunctionType({ } typeIn1, { } typeOut1), FunctionType({ } typeIn2, { } typeOut2)):
                {
                    var result1 = MostGeneralUnifier(typeIn1, typeIn2);
                    if (result1 is (Substitution sub1, _))
                    {
                        var result2 = MostGeneralUnifier(ApplySubstitution(typeOut1, sub1), ApplySubstitution(typeOut2, sub1));
                        if (result2 is (Substitution sub2, _))
                        {
                            return TypeInferenceResult.Ok(ComposeSubstitutions(sub1, sub2));
                        }
                        else
                            return result2; // error
                    }
                    else
                        return result1; // error
                }
            // If one of the types is variable, we can bind the variable to the type.
            // This also handles the case where they are both variables.
            case (VariableType({ } v), { } t):
                {
                    return BindVariable(v, t);
                }
            case ({ } t, VariableType({ } v)):
                {
                    return BindVariable(v, t);
                }
            // If they are both primitives, no substitution needs to be done.
            case (IntegerType, IntegerType) or (FloatType, FloatType) or (BoolType, BoolType) or (StringType, StringType):
                {
                    return TypeInferenceResult.Ok(Substitution.Empty());
                }
            // Otherwise, the types cannot be unified.
            case (var t1, var t2):
                {
                    return TypeInferenceResult.Fail<Substitution>(new TypeInferenceError($"types do not unify: {t1} vs {t2}"));
                }
        }
    }

    /// <summary>
    /// Attempt to bind a type variable to a type, returning an appropriate substitution.
    /// </summary>
    private static TypeInferenceResult<Substitution> BindVariable(TypeVar typeVar, InferredType type)
    {
        // Check for binding a variable to itself
        if (type is VariableType({ } u) && u == typeVar)
        {
            return TypeInferenceResult.Ok(Substitution.Empty());
        }

        // The occurs check prevents illegal recursive types.
        if (GetFreeTypeVariables(type).Contains(typeVar))
        {
            return TypeInferenceResult.Fail<Substitution>(new TypeInferenceError($"occur check fails: {typeVar} vs {type}"));
        }

        var s = Substitution.Empty().Insert(typeVar, type);
        return TypeInferenceResult.Ok(s);
    }

    private static HashSet<TypeVar> GetFreeTypeVariables(InferredType type)
    {
        return type switch
        {
            // For a type variable, there is one free variable: the variable itself.
            VariableType({ } s) => new HashSet<TypeVar>(new[] { s }),
            // Primitive types have no free variables
            IntegerType or FloatType or BoolType or StringType => new HashSet<TypeVar>(),
            // For functions, we take the union of the free type variables of the input and output.
            FunctionType({ } inType, { } outType) => union(GetFreeTypeVariables(inType), GetFreeTypeVariables(outType)),
            _ => throw new InvalidOperationException($"unexpected type '{type}'")
        };

        HashSet<TypeVar> union(HashSet<TypeVar> a, HashSet<TypeVar> b)
        {
            a.UnionWith(b);
            return a;
        }
    }

    /// <summary>
    /// The free type variables in a polytype are those that are free in the internal type and not bound by the variable mapping.
    /// </summary>
    private static HashSet<TypeVar> GetFreeTypeVariables(Polytype polytype)
    {
        // The free type variables in a polytype are those that are free in the internal type and not bound by the variable mapping.
        var set = GetFreeTypeVariables(polytype.Type);
        set.ExceptWith(polytype.TypeVariables);
        return set;
    }

    /// <summary>
    /// The free type variables of a type environment is the union of the free type variables of each polytype in the environment.
    /// </summary>
    private static HashSet<TypeVar> GetFreeTypeVariables(TypeEnvironment typeEnv)
    {
        return GetFreeTypeVariables(typeEnv.Enumerate().Select(kv => kv.Value).ToList());
    }

    /// <summary>
    /// The free type variables of a vector of types is the union of the free type variables of each of the types in the vector.
    /// </summary>
    private static HashSet<TypeVar> GetFreeTypeVariables(IReadOnlyList<Polytype> types)
    {
        var union = new HashSet<TypeVar>();
        foreach(var type in types)
        {
            union.UnionWith(GetFreeTypeVariables(type));
        }
        return union;
    }

    /// <summary>
    /// Instantiates a polytype into a type. Replaces all bound type variables with fresh type variables and return the resulting type.
    /// </summary>
    private static InferredType InstantiateType(Polytype polytype, TypeVarGenerator typeVarGenerator)
    {
        var newVarMap = polytype.TypeVariables.Select(typeVar => (typeVar, newVar: new VariableType(typeVarGenerator.GetNext()))).ToImmutableDictionary(x => x.typeVar, x => (InferredType)x.newVar);
        var substitution = new Substitution(newVarMap);
        return ApplySubstitution(polytype.Type, substitution);
    }

    public TypeEnvironment GetDefaultTypeEnvironment()
    {
        var typeEnvironment = TypeEnvironment.Empty()
            .Insert("+", new Polytype(Array.Empty<TypeVar>(), new FunctionType(new IntegerType(), new FunctionType(new IntegerType(), new IntegerType())))) // + :: int -> int -> int -- binary addition
            .Insert("-", new Polytype(Array.Empty<TypeVar>(), new FunctionType(new IntegerType(), new IntegerType()))); // - :: int -> int -- unary negation
        return typeEnvironment;
    }

    public TypeInferenceResult<InferredType> Run(Expression expression, TypeEnvironment typeEnvironment, TypeVarGenerator typeVarGenerator)
    {
        return InferType(expression, typeEnvironment, typeVarGenerator) switch
        {
            ((Substitution substitution, InferredType type), _) => new(ApplySubstitution(type, substitution), null),
            (_, TypeInferenceError error) => new(null, error)
        };
    }
}
