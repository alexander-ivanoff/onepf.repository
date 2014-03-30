package org.onepf.repository;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import java.io.IOException;

/**
 * Created by ivanoff on 28.03.14.
 */
public class XmlFileFilter implements Filter {

    public static String FILTER_APPLICATIONS = "applist_";
    public static String FILTER_XML = ".xml";

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {

    }

    @Override
    public void doFilter(final ServletRequest servletRequest, final ServletResponse servletResponse, FilterChain filterChain) throws IOException, ServletException {
        HttpServletRequest httpServletRequest = (HttpServletRequest) servletRequest;
        String servletPath = httpServletRequest.getServletPath();
        if (servletPath.endsWith(FILTER_XML)) {

            String[] pathParts = servletPath.substring(0, servletPath.length() - FILTER_XML.length()).split("/");
            String lastPart = pathParts[pathParts.length - 1];

            if (lastPart.startsWith(FILTER_APPLICATIONS)) {
                final String pageHash =  lastPart.substring(FILTER_APPLICATIONS.length());
                System.out.println("TEST2: " + lastPart.substring(FILTER_APPLICATIONS.length()));
                final RequestDispatcher dispatcher = httpServletRequest.getRequestDispatcher("/openaep/applist");

                ServletRequest wrapper = new HttpServletRequestWrapper(httpServletRequest) {
                    @Override
                    public String getParameter(String name) {
                        if (name.equals("page")) {
                            return pageHash;
                        }
                        return super.getParameter(name);
                    }
                };
                dispatcher.forward(wrapper, servletResponse);
                return;
            }
        }
        filterChain.doFilter(servletRequest, servletResponse);
    }

    @Override
    public void destroy() {

    }
}
