package org.onepf.repository.appstorelooter;

import org.onepf.repository.ApiMapping;
import org.onepf.repository.api.responsewriter.WriteException;
import org.onepf.repository.api.responsewriter.entity.*;
import org.onepf.repository.api.xmlapi.XmlResponseReaderWriter;

/**
 * Feed type enum.
 *
 * @author  Ruslan Sayfutdinov
 */
public enum FeedType {
    DOWNLOADS("downloads", ApiMapping.LIST_DOWNLOADS, DownloadListEntity.class, DownloadEntity.class),
    REVIEWS("reviews", ApiMapping.LIST_REVIEWS, ReviewsListEntity.class, ReviewEntity.class),
    PURCHASES("purchases", ApiMapping.LIST_PURCHASES, PurchaseListEntity.class, PurchaseEntity.class);

    private final String type;
    private final ApiMapping apiMapping;
    private final Class listEntity;
    private final Class elementEntity;

    FeedType(String type, ApiMapping mapping, Class listEntity, Class elementEntity) {
        this.type = type;
        this.apiMapping = mapping;
        this.listEntity = listEntity;
        this.elementEntity = elementEntity;
    }

    public static FeedType fromString(String feedType) {
        for (FeedType type : FeedType.values()) {
            if (feedType.equals(type.type)) {
                return type;
            }
        }
        throw new IllegalArgumentException("No FeedType with type " + feedType + " found");
    }

    @Override
    public String toString() {
        return type;
    }

    public ApiMapping getApiMapping() { return apiMapping; }

    public Class getListEntity() { return listEntity; }

    public Class getElementEntity() { return elementEntity; }

    public XmlResponseReaderWriter getXmlReaderWriter() {
        try {
            if (type.equals(DOWNLOADS.toString())) {
                return new XmlResponseReaderWriter<DownloadListEntity>(ObjectFactory._Downloads_QNAME, listEntity.getPackage().getName());
            } else if (type.equals(PURCHASES.toString())) {
                return new XmlResponseReaderWriter<PurchaseListEntity>(ObjectFactory._Purchases_QNAME, listEntity.getPackage().getName());
            } else if (type.equals(REVIEWS.toString())) {
                return new XmlResponseReaderWriter<ReviewsListEntity>(ObjectFactory._Reviews_QNAME, listEntity.getPackage().getName());
            }
        } catch (WriteException e) {
            e.printStackTrace();
        }
        return null;
    }
}
