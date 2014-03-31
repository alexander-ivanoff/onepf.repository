package org.onepf.repository;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by ivanoff on 28.03.14.
 */
public class XmlFileFilter implements Filter {

    private static class CustomizedRequestWrapper extends HttpServletRequestWrapper{

        private Map<String, String> customizedParameters;

        CustomizedRequestWrapper(HttpServletRequest servletRequest) {
            super(servletRequest);
            customizedParameters = new HashMap<String, String>();
        }

        public void addParameter(String name, String value) {
            customizedParameters.put(name, value);
        }

        @Override
        public String getParameter(String name) {
            String parameterValue = customizedParameters.get(name);
            if (parameterValue == null) {
                parameterValue = super.getParameter(name);
            }
            return parameterValue;
        }
    }


    public static String FILTER_APPLICATIONS = "applist";
    public static String FILTER_DOWNLOADS = "downloads";
    public static String FILTER_PURCHASES = "purchases";
    public static String FILTER_REVIEWS = "reviews";
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

            String[] lastPartParts = lastPart.split("_");

            RequestDispatcher dispatcher = null;
            CustomizedRequestWrapper wrapper = new CustomizedRequestWrapper(httpServletRequest);

            if (lastPartParts[0].equals(FILTER_APPLICATIONS)) {
                dispatcher = httpServletRequest.getRequestDispatcher("/openaep/applist");
                wrapper.addParameter("page", lastPartParts[1]); // index of page hash
            } else {
                if (lastPartParts[0].equals(FILTER_DOWNLOADS)) {
                    dispatcher = httpServletRequest.getRequestDispatcher("/openaep/downloads");
                } else if (lastPartParts[0].equals(FILTER_PURCHASES)) {
                    dispatcher = httpServletRequest.getRequestDispatcher("/openaep/purchases");
                } else if (lastPartParts[0].equals(FILTER_REVIEWS)) {
                    dispatcher = httpServletRequest.getRequestDispatcher("/openaep/reviews");
                }
                wrapper.addParameter("package", lastPartParts[1]);
                wrapper.addParameter("page", lastPartParts[2]); // index of page hash

            }
            if (dispatcher != null) {
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
