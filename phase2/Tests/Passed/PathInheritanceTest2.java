package Tests.Passed;

class OuterClass2 {

}

class OuterClass
{ 
    static class InnerClassOne extends OuterClass2 {}
    {
    }
}
 

class AnotherClassOne extends OuterClass.InnerClassOne
{
}