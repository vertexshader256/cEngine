package java_api.c.engine;

import scala.c.engine.State;
import scala.c.engine.gcc.Gcc;
import scala.collection.JavaConverters;

import java.util.ArrayList;
import java.util.Iterator;

public class JavaAPI {
    public void runC(String code, State state) {
        Iterator<String> itr = new ArrayList<String>().iterator();
        Gcc.runCode(code, state, JavaConverters.asScalaIteratorConverter(itr).asScala());
    }
}
