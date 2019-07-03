package Tests.Passed;


public class ClassDupConstructorsTest extends B {
    String name;

    ClassDupConstructorsTest(String name) {
        this.name = name;
    }

    public ClassDupConstructorsTest(int value) {
    }

    ClassDupConstructorsTest() {
    }
    
    private int a(String l) {
		return 3;
	}
	

}

abstract class B {}