package java_api.c.engine;

import scala.c.engine.CEngine;
import scala.collection.JavaConverters;

import java.util.ArrayList;
import java.util.Iterator;

public class JavaAPI {
    public void runC(String code, CEngine state) {
        Iterator<String> itr = new ArrayList<String>().iterator();
        state.runCode(code, JavaConverters.asScalaIteratorConverter(itr).asScala());
    }
}
