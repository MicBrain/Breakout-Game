import java.util*;

public class Test {

	private int testAttr_1;
	private String testAttr_2;
	private ArrayList<String> testAttr_3;

	public TestConstructor() {
		//do nothing
	}

	public TestConstructor2(int input_num, String input_str, ArrayList<Stirng> input_ArrayList) {
		testAttr_1 = input_num;
		testAttr_2 = input_str;
		testAttr_3 = input_ArrayList;
	}

	public void printMessage() {
		System.out.println("This is a test file in JAVA");
	}

	public void factorial(int num) {
		if (num == 0) {
			return 1;
		}else {
			return num * factorial(num-1);
		}
	}
}