import java.io.*;

class Nuke2 {
    public static void main(String[] arg) throws Exception {
	String outputLine;

	BufferedReader keyboard = 
	    new BufferedReader(new InputStreamReader(System.in));
	outputLine = keyboard.readLine();
	System.out.println(outputLine.substring(0,1) + outputLine.substring(2));
    }
}
