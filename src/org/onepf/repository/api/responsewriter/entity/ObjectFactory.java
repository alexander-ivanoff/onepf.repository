//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.4-2 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2014.04.17 at 12:45:26 PM MSK 
//


package org.onepf.repository.api.responsewriter.entity;


import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the org.onepf.repository.api.xmlapi.applist package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    public final static QName _ApplicationList_QNAME = new QName("", "application-list");
    public final static QName _Downloads_QNAME = new QName("", "downloads");
    public final static QName _Purchases_QNAME = new QName("", "purchases");
    public final static QName _Reviews_QNAME = new QName("", "reviews");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: org.onepf.repository.api.xmlapi.applist
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link org.onepf.repository.api.responsewriter.entity.ApplicationListEntity }
     * 
     */
    public ApplicationListEntity createApplicationListType() {
        return new ApplicationListEntity();
    }

    /**
     * Create an instance of {@link org.onepf.repository.api.responsewriter.entity.ApplicationEntity }
     * 
     */
    public ApplicationEntity createApplicationType() {
        return new ApplicationEntity();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link org.onepf.repository.api.responsewriter.entity.ApplicationListEntity }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "application-list")
    public JAXBElement<ApplicationListEntity> createApplicationList(ApplicationListEntity value) {
        return  value.getAsJaxbElement(_ApplicationList_QNAME);
    }


    /**
     * Create an instance of {@link org.onepf.repository.api.responsewriter.entity.DownloadListEntity }
     *
     */
    public DownloadListEntity createDownloadsType() {
        return new DownloadListEntity();
    }

    /**
     * Create an instance of {@link org.onepf.repository.api.responsewriter.entity.DownloadEntity }
     *
     */
    public DownloadEntity createDownloadType() {
        return new DownloadEntity();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link org.onepf.repository.api.responsewriter.entity.DownloadListEntity }{@code >}}
     *
     */
    @XmlElementDecl(namespace = "", name = "downloads")
    public JAXBElement<DownloadListEntity> createDownloads(DownloadListEntity value) {
        return value.getAsJaxbElement(_Downloads_QNAME);
    }

    /**
     * Create an instance of {@link org.onepf.repository.api.responsewriter.entity.PurchaseListEntity }
     *
     */
    public PurchaseListEntity createPurchasesType() {
        return new PurchaseListEntity();
    }

    /**
     * Create an instance of {@link org.onepf.repository.api.responsewriter.entity.PurchaseEntity }
     *
     */
    public PurchaseEntity createPurchaseType() {
        return new PurchaseEntity();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link org.onepf.repository.api.responsewriter.entity.PurchaseListEntity }{@code >}}
     *
     */
    @XmlElementDecl(namespace = "", name = "purchases")
    public JAXBElement<PurchaseListEntity> createPurchases(PurchaseListEntity value) {
        return new JAXBElement<PurchaseListEntity>(_Purchases_QNAME, PurchaseListEntity.class, null, value);
    }

    /**
     * Create an instance of {@link org.onepf.repository.api.responsewriter.entity.ReviewsListEntity }
     *
     */
    public ReviewsListEntity createReviewsType() {
        return new ReviewsListEntity();
    }

    /**
     * Create an instance of {@link org.onepf.repository.api.responsewriter.entity.ReviewEntity }
     *
     */
    public ReviewEntity createReviewType() {
        return new ReviewEntity();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link org.onepf.repository.api.responsewriter.entity.ReviewsListEntity }{@code >}}
     *
     */
    @XmlElementDecl(namespace = "", name = "reviews")
    public JAXBElement<ReviewsListEntity> createReviews(ReviewsListEntity value) {
        return new JAXBElement<ReviewsListEntity>(_Reviews_QNAME, ReviewsListEntity.class, null, value);
    }

}
