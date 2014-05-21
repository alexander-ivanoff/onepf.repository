package org.onepf.repository;

import org.onepf.repository.api.responsewriter.ResponseReaderWriter;
import org.onepf.repository.api.responsewriter.WriteException;
import org.onepf.repository.api.responsewriter.entity.ObjectFactory;
import org.onepf.repository.api.responsewriter.entity.ReviewEntity;
import org.onepf.repository.api.responsewriter.entity.ReviewsListEntity;
import org.onepf.repository.api.xmlapi.XmlResponseReaderWriter;
import org.onepf.repository.model.services.SimpleListRequestHandler;

import java.util.List;

/**
 *
 * This Servlet returns list of reviews for all packages provided by requesting appstore.
 *
* @author Alexander Ivanoff on 11.03.14.
 */
public class GetReviewListServlet extends SimpleListServlet<ReviewEntity, ReviewsListEntity> {


    @Override
    String initOffsetTemplate() {
        return "reviews_%d.xml";
    }

    @Override
    ResponseReaderWriter initResponseWriter() {
        try {
            return new XmlResponseReaderWriter(ObjectFactory._Reviews_QNAME, ReviewsListEntity.class.getPackage().getName());
        } catch (WriteException e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    SimpleListRequestHandler<ReviewEntity> initRequestHandler() {
        return getRepositoryFactory().createReviewsHandler();
    }

    @Override
    public ReviewsListEntity buildListEntity(List<ReviewEntity> entities) {
        ReviewsListEntity listEntity = new ReviewsListEntity();
        listEntity.getReview().addAll(entities);
        return listEntity;
    }
}
