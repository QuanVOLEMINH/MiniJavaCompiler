package Tests.Failed;

public class MethodIllegalBodyDefTest extends B {

	MethodIllegalBodyDefTest() {

	}
	int sum(double d){}

}

abstract class B {
    abstract void testM();
}