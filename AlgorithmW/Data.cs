using System;
using System.Collections.Immutable;

namespace AlgorithmW;

/// <summary>
/// A term variable is a variable referenced in an expression
/// </summary>
public record TermVar(string Name)
{
    public static implicit operator string(TermVar term) => term.Name;
    public static implicit operator TermVar(string s) => new(s);
    public override string ToString() => Name;
}

public abstract record Expression;


public record Variable(TermVar Term) : Expression
{
    public override string ToString() => Term;
}

/// <summary>
/// A literal value of some primitive
/// </summary>
public abstract record Literal : Expression;
public record IntegerLiteral(int Value) : Literal
{
    public override string ToString() => Value.ToString();
}
public record FloatLiteral(double Value) : Literal
{
    public override string ToString() => Value.ToString();
}
public record BoolLiteral(bool Value) : Literal
{
    public override string ToString() => Value.ToString();
}
public record StringLiteral(string Value) : Literal
{
    public override string ToString() => $"\"{Value}\"";
}

public record ApplicationExpression(Expression Exp1, Expression Exp2) : Expression
{
    public override string ToString() => $"({Exp1} {Exp2})";
}

public record AbstractionExpression(TermVar Term, Expression Exp) : Expression
{
    //public override string ToString() => $"λ{Term}.{Exp}";
    public override string ToString() => $"{Term} => {Exp}";
}

public record LetExpression(TermVar Term, Expression Exp1, Expression Exp2) : Expression
{
    public override string ToString() => $"(let {Term} = {Exp1} in {Exp2})";
}


public record TypeVar(int ID)
{
    public override string ToString() => $"'t{ID}";
}

public abstract record InferredType;
public record VariableType(TypeVar TypeVar) : InferredType
{
    public override string ToString() => TypeVar.ToString();
}
public record IntegerType : InferredType
{
    public override string ToString() => "Int";
}
public record FloatType : InferredType
{
    public override string ToString() => "Float";
}
public record BoolType : InferredType
{
    public override string ToString() => "Bool";
}
public record StringType : InferredType
{
    public override string ToString() => "String";
}
public record FunctionType(InferredType TypeIn, InferredType TypeOut) : InferredType
{
    public override string ToString() => $"({TypeIn} → {TypeOut})";
}

public record Polytype(IReadOnlyList<TypeVar> TypeVariables, InferredType Type);

public record TypeInferenceError(string Message)
{
    public override string ToString() => Message;
}
public static class TypeInferenceResult
{
    public static TypeInferenceResult<TResult> Ok<TResult>(TResult result) => new(result, null);
    public static TypeInferenceResult<TResult> Fail<TResult>(TypeInferenceError error) => new(default, error);
}
public record TypeInferenceResult<TResult>(TResult? Result, TypeInferenceError? Error);

public class Substitution
{
    private readonly ImmutableDictionary<TypeVar, InferredType> map;
    public static Substitution Empty() => new(ImmutableDictionary<TypeVar, InferredType>.Empty);
    public Substitution(ImmutableDictionary<TypeVar, InferredType> map) => this.map = map;
    public InferredType? TryGet(TypeVar typeVar) => map.TryGetValue(typeVar, out var type) ? type : null;
    public Substitution Insert(TypeVar typeVar, InferredType type) => new(map.Add(typeVar, type));
    public Substitution Remove(TypeVar typeVar) => new(map.Remove(typeVar));
    public IEnumerable<KeyValuePair<TypeVar, InferredType>> Enumerate() => map;
    public Substitution UnionWith(Substitution other)
    {
        var union = this.map;
        foreach(var kv in other.map)
        {
            if (!this.map.ContainsKey(kv.Key))
            {
                union = union.Add(kv.Key, kv.Value);
            }
        }
        return new Substitution(union);
    }
}

public class TypeEnvironment
{
    private readonly ImmutableDictionary<TermVar, Polytype> map;

    public TypeEnvironment(IEnumerable<(TermVar, Polytype)> values) => map = values.ToImmutableDictionary(kv => kv.Item1, kv => kv.Item2);

    private TypeEnvironment(ImmutableDictionary<TermVar, Polytype> map) => this.map = map;

    public Polytype? TryGet(TermVar term) => map.TryGetValue(term, out var polytype) ? polytype : null;

    public static TypeEnvironment Empty() => new(ImmutableDictionary<TermVar, Polytype>.Empty);
    public TypeEnvironment Insert(TermVar typeVar, Polytype polytype) => new(map.Add(typeVar, polytype));
    public TypeEnvironment Remove(TermVar term) => new TypeEnvironment(map.Remove(term));
    public IEnumerable<KeyValuePair<TermVar, Polytype>> Enumerate() => map;
}

public class TypeVarGenerator
{
    private int supply = 0;
    public TypeVar GetNext() => new TypeVar(supply++);
}