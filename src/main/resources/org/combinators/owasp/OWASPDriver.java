package org.combinators.owasp;

import de.tudo.ls14.jdart.owasp.mocking.HttpServletRequest;
import de.tudo.ls14.jdart.owasp.mocking.HttpServletResponse;
import de.tudo.ls14.jdart.owasp.mocking.Cookie;
import de.tudo.ls14.jdart.owasp.mocking.HttpServlet;

import javax.servlet.http.HttpServletCaller;

import java.io.IOException;
import java.util.Random;
import javax.servlet.ServletException;

public class OWASPDriver {

    public String createSymbString(Random r) {
        //TODO: NegativeArraySizeException if int length = r.nextInt(); investigate, why not catched by jDart.
        //It seems like the full choice generation is not exploited here.
        int length = r.nextInt(7000);
        byte[] bytes = new byte[length];
        r.nextBytes(bytes);
        String value = new String(bytes);
        return value;
    }



    private String createSymbStringFixedLength(Random r, int length){
        byte[] bytes = new byte[length];
        r.nextBytes(bytes);
        String value = new String(bytes);
        return value;
    }

    public static void main(String[] args) throws Throwable {
        System.out.println("main - top");
        OWASPDriver benchmark = new OWASPDriver();
        System.out.println("main - constructor");
        benchmark.testPost();
    }
}