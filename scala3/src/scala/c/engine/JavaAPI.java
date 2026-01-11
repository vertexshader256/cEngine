package scala.c.engine;

import scala.collection.JavaConverters;

import java.util.ArrayList;
import java.util.Iterator;
import scala.c.engine.gcc.Gcc;

public class JavaAPI {
    public void runC(String code, State state) {
        Iterator<String> itr = new ArrayList<String>().iterator();
        Gcc.runCode(code, state, JavaConverters.asScalaIteratorConverter(itr).asScala());
    }
}
