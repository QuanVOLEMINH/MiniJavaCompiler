package Tests.Failed;

public class MethodDupDefinitionTest extends B {

	MethodDupDefinitionTest() {

	}
	private int a(int b) {
		return 0;
	}

	void a(int c){}
	

}

class B {}