package scala.c.engine;

import java.util.ArrayList;
import java.util.Iterator;

import scala.collection.JavaConverters;

public class JavaAPI {
    public void runC(String code, State state) {
        Iterator<String> itr = new ArrayList<String>().iterator();
        Gcc.runCode(code, state, JavaConverters.asScalaIteratorConverter(itr).asScala());
    }
}
