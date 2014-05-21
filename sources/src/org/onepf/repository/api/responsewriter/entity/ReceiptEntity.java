package org.onepf.repository.api.responsewriter.entity;

import org.onepf.repository.api.xmlapi.JaxbElementMaker;

import javax.persistence.*;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.namespace.QName;

/**
 * Created by ivanoff on 05.05.14.
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "receiptType")
@Entity
@Table(name = "receipts")
public class ReceiptEntity extends BaseEntity implements JaxbElementMaker{

    @Id
    @GeneratedValue(strategy= GenerationType.AUTO)
    private int id;

    @XmlAttribute(name = "receipt-data", required = true)
    @Column(name = "receiptData")
    String receiptData;

    @XmlAttribute(name = "distributor-appstore", required = false)
    @Column(name = "distributorStoreId")
    String distributorAppstore;

    @XmlAttribute(name = "distributor-signature", required = false)
    @Column(name = "distributorSignature")
    String distributorSignature;

    @XmlAttribute(name = "developer-appstore", required = false)
    @Column(name = "developerStoreId")
    String developerAppstore;

    @XmlAttribute(name = "developer-signature", required = false)
    @Column(name = "developerSignature")
    String developerSignature;

    @XmlAttribute(name = "datetime")
    @Column(name = "datetime")
    private String dateTime;

    @Override
    public  JAXBElement<ReceiptEntity> getAsJaxbElement(QName qname) {
        return new JAXBElement<ReceiptEntity>(qname, ReceiptEntity.class, null, this);
    }

    public String getReceiptData() {
        return receiptData;
    }

    public void setReceiptData(String receiptData) {
        this.receiptData = receiptData;
    }

    public String getDistributorAppstore() {
        return distributorAppstore;
    }

    public void setDistributorAppstore(String distributorAppstore) {
        this.distributorAppstore = distributorAppstore;
    }

    public String getDistributorSignature() {
        return distributorSignature;
    }

    public void setDistributorSignature(String distributorSignature) {
        this.distributorSignature = distributorSignature;
    }

    public String getDeveloperAppstore() {
        return developerAppstore;
    }

    public void setDeveloperAppstore(String developerAppstore) {
        this.developerAppstore = developerAppstore;
    }

    public String getDeveloperSignature() {
        return developerSignature;
    }

    public void setDeveloperSignature(String developerSignature) {
        this.developerSignature = developerSignature;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getDateTime() {
        return dateTime;
    }

    public void setDateTime(String dateTime) {
        this.dateTime = dateTime;
    }
}
