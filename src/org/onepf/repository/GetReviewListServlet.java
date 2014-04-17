package org.onepf.repository;

import org.onepf.repository.api.responsewriter.ResponseWriterV2;
import org.onepf.repository.api.responsewriter.entity.*;
import org.onepf.repository.api.xmlapi.XmlResponseWriterV2;
import org.onepf.repository.model.GetReviewsRequestHandler;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.api.responsewriter.WriteException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

/**
 *
 * This Servlet returns list of reviews for all packages provided by requesting appstore.
 *
* @author Alexander Ivanoff on 11.03.14.
 */
public class GetReviewListServlet extends BaseServlet {

    private static final String PARAMETER_PAGE = "page";

    public final static String FILE_PREFIX = "reviews";
    private final static String FILE_TEMPLATE = FILE_PREFIX + "_%d.xml";

    private static final ResponseWriterV2 responseWriter = initResponseWriter();

    private static ResponseWriterV2 initResponseWriter() {
        XmlResponseWriterV2 responseWriter = null;
        try {
            responseWriter = new XmlResponseWriterV2(ObjectFactory._Downloads_QNAME, DownloadListEntity.class.getPackage().getName());
        } catch (WriteException e) {
            e.printStackTrace();
        }
        return responseWriter;
    }

    private GetReviewsRequestHandler handler;

    @Override
    public void init() throws ServletException {
        super.init();
        handler = getRepositoryFactory().createReviewsHandler();
    }

    protected void post(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    }

    protected void get(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        try {
            String page = request.getParameter(PARAMETER_PAGE);
            List<ReviewEntity> reviews = handler.getReviews(getAppstore(request).appstoreId, page != null ? Integer.valueOf(page) : -1);
            String offset = getOffset(request, reviews);
            responseWriter.write(response.getOutputStream(), buildListEntity("1", offset, reviews));
        } catch (WriteException e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (DataException e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

    private static String getOffset(HttpServletRequest request, List<ReviewEntity> reviews) {
        String offset = null;
        if (reviews.size() > 0 ) {
            ReviewEntity lastReview = reviews.get(0);
            if (lastReview.getCurrPageHash() != lastReview.getPrevPageHash()) {
                offset = String.format(FILE_TEMPLATE, lastReview.getPrevPageHash());
            }
        }
        return offset;
    }

    private static ReviewsListEntity buildListEntity(String version, String offset, List<ReviewEntity> reviews) {
        ReviewsListEntity listEntity = new ReviewsListEntity();
        listEntity.setVersion(version);
        listEntity.setOffset(offset);
        listEntity.getReview().addAll(reviews);
        return listEntity;
    }

}
