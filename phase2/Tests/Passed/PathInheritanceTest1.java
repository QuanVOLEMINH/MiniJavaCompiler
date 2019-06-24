package abc.xyz.ghj;

class OuterClass
{
    static class InnerClassOne
    {
    }
 
    static class InnerClassTwo extends InnerClassOne
    {
    }
}
 
class AnotherClassOne extends OuterClass.InnerClassOne
{
}
 
// class AnotherClassTwo extends OuterClass.InnerClassTwo
// {
// }