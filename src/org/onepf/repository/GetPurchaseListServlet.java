package org.onepf.repository;

import org.onepf.repository.api.responsewriter.ResponseReaderWriter;
import org.onepf.repository.api.responsewriter.WriteException;
import org.onepf.repository.api.responsewriter.entity.ObjectFactory;
import org.onepf.repository.api.responsewriter.entity.PurchaseEntity;
import org.onepf.repository.api.responsewriter.entity.PurchaseListEntity;
import org.onepf.repository.api.xmlapi.XmlResponseReaderWriter;
import org.onepf.repository.model.services.SimpleListRequestHandler;

import java.util.List;

/**
 *
 * This Servlet returns list of purchases for all packages provided by requesting appstore.
 *
* @author Alexander Ivanoff on 11.03.14.
 */
public class GetPurchaseListServlet extends SimpleListServlet<PurchaseEntity, PurchaseListEntity> {

    @Override
    String initOffsetTemplate() {
        return "purchases_%d.xml";
    }

    @Override
    ResponseReaderWriter initResponseWriter() {
        try {
            return new XmlResponseReaderWriter(ObjectFactory._Purchases_QNAME, PurchaseListEntity.class.getPackage().getName());
        } catch (WriteException e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    SimpleListRequestHandler<PurchaseEntity> initRequestHandler() {
        return getRepositoryFactory().createPurchasesHandler();
    }

    @Override
    public PurchaseListEntity buildListEntity(List<PurchaseEntity> entities) {
        PurchaseListEntity listEntity = new PurchaseListEntity();
        listEntity.getPurchase().addAll(entities);
        return listEntity;
    }
}
