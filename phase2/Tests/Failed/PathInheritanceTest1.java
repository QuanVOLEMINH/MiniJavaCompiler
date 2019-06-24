package abc.xyz.ghj;

class OuterClass
{
	// non-static
    class InnerClassOne
    {
    }
 
    static class InnerClassTwo extends InnerClassOne
    {
    }
}

// ok 
class AnotherClassOne extends OuterClass.InnerClassOne
{
}

// failed
class AnotherClassTwo extends OuterClass.InnerClassTwo
{
}