package org.onepf.repository;

import org.onepf.repository.model.GetReviewsRequestHandler;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.utils.responsewriter.ResponseWriter;
import org.onepf.repository.utils.responsewriter.WriteException;
import org.onepf.repository.utils.responsewriter.XmlResponseWriter;
import org.onepf.repository.utils.responsewriter.descriptors.ReviewDescriptor;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

/**
 * Created by ivanoff on 11.03.14.
 */
public class GetReviewListServlet extends BaseServlet {

    private static final String PARAMETER_PACKAGE = "package";
    private static final String PARAMETER_PAGE = "page";

    public final static String FILE_PREFIX = "reviews";
    private final static String FILE_TEMPLATE = FILE_PREFIX + "_%s_%d.xml";

    private GetReviewsRequestHandler handler;

    @Override
    public void init() throws ServletException {
        super.init();
        handler = getRepositoryFactory().createReviewsHandler();
    }

    protected void post(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    }

    protected void get(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        String packageName = request.getParameter(PARAMETER_PACKAGE);
        if (packageName == null) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "No package defined");
            return;
        }

        try {
            String page = request.getParameter(PARAMETER_PAGE);
            List<ReviewDescriptor> reviews = handler.getReviews(packageName, page != null ? Integer.valueOf(page) : -1);
            ResponseWriter responseWriter = new XmlResponseWriter();
            String prevFileLink = null;
            String lastUpdate = null;
            if (reviews.size() > 0 ) {
                ReviewDescriptor lastReview = reviews.get(0);
                lastUpdate = lastReview.lastUpdate;
                if (lastReview.currPageHash != lastReview.prevPageHash) {
                    prevFileLink = String.format(FILE_TEMPLATE, packageName, lastReview.prevPageHash);
                }

            }
            responseWriter.writeReviews(response.getWriter(), reviews, lastUpdate, prevFileLink);
        } catch (WriteException e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (DataException e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

}