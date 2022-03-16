global using static AlgorithmW.HelperFunctions;

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;


namespace AlgorithmW;


public static class HelperFunctions
{
    public static Expression bind(string name, Expression e1, Expression e2)
    {
        return new LetExpression(name, e1, e2);
    }

    public static Expression abs(string var, Expression e)
    {
        return new AbstractionExpression(var, e);
    }

    public static Expression app(Expression e1, Expression e2)
    {
        return new ApplicationExpression(e1, e2);
    }

    public static Expression lit(int value)
    {
        return new IntegerLiteral(value);
    }

    public static Expression lit(double value)
    {
        return new FloatLiteral(value);
    }

    public static Expression lit(bool value)
    {
        return new BoolLiteral(value);
    }

    public static Expression lit(string value)
    {
        return new StringLiteral(value);
    }

    public static Expression var(string name)
    {
        return new Variable(name);
    }
}
