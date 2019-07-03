package Tests.Passed;

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