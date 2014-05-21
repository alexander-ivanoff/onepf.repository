package org.onepf.repository.api.responsewriter.entity;

import javax.persistence.*;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

/**
 * Created by akarimova on 16.04.14.
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "purchaseType")
@Entity
@Table(name = "purchases")
public class PurchaseEntity extends BaseStatisticEntity{

    @XmlAttribute(name = "id")
    @Column(name = "purchaseId")
    private String purchaseId;

    @XmlAttribute(name = "user-price")
    @Column(name = "userPrice")
    private String userPrice;

    @XmlAttribute(name = "user-currency")
    @Column(name = "userCurrency")
    private String userCurrency;

    @XmlAttribute(name = "inner-price")
    @Column(name = "innerPrice")
    private String innerPrice;

    @XmlAttribute(name = "inner-currency")
    @Column(name = "innerCurrency")
    private String innerCurrency;

    @XmlAttribute(name = "signature")
    @Column(name = "signature")
    private String signature;


    public String getUserPrice() {
        return userPrice;
    }

    public void setUserPrice(String userPrice) {
        this.userPrice = userPrice;
    }

    public String getUserCurrency() {
        return userCurrency;
    }

    public void setUserCurrency(String userCurrency) {
        this.userCurrency = userCurrency;
    }

    public String getInnerPrice() {
        return innerPrice;
    }

    public void setInnerPrice(String innerPrice) {
        this.innerPrice = innerPrice;
    }

    public String getInnerCurrency() {
        return innerCurrency;
    }

    public void setInnerCurrency(String innerCurrency) {
        this.innerCurrency = innerCurrency;
    }

    public String getSignature() {
        return signature;
    }

    public void setSignature(String signature) {
        this.signature = signature;
    }

    public String getPurchaseId() {
        return purchaseId;
    }

    public void setPurchaseId(String purchaseId) {
        this.purchaseId = purchaseId;
    }
}
