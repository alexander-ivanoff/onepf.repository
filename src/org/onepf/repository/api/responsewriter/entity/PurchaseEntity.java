package org.onepf.repository.api.responsewriter.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
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
public class PurchaseEntity extends BaseEntity{

    @XmlAttribute(name = "package")
    @Column(name = "package")
    private String packageName;

    @XmlAttribute(name = "datetime")
    @Column(name = "datetime")
    private String dateTime;

    @XmlAttribute(name = "id")
    @javax.persistence.Id
    private int id;

    @XmlAttribute(name = "version")
    @Column(name = "version")
    private String version;

    @XmlAttribute(name = "versionCode")
    @Column(name = "versionCode")
    private int build;

    @Column(name = "lastUpdate")
    private String lastUpdate;

    @XmlAttribute(name = "device-model")
    @Column(name = "deviceModel")
    private String deviceModel;

    @XmlAttribute(name = "device-name")
    @Column(name = "deviceName")
    private String deviceName;

    @XmlAttribute(name = "country")
    @Column(name = "country")
    private String country;

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

    @XmlAttribute(name = "appstoreId", required = true)
    @Column(name = "distributorStoreId")
    private String distributorStoreId;

    @Column(name = "homeStoreId")
    private String homeStoreId;

    public String getPackageName() {
        return packageName;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public String getDateTime() {
        return dateTime;
    }

    public void setDateTime(String dateTime) {
        this.dateTime = dateTime;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public int getBuild() {
        return build;
    }

    public void setBuild(int build) {
        this.build = build;
    }

    public String getLastUpdate() {
        return lastUpdate;
    }

    public void setLastUpdate(String lastUpdate) {
        this.lastUpdate = lastUpdate;
    }

    public String getDeviceModel() {
        return deviceModel;
    }

    public void setDeviceModel(String deviceModel) {
        this.deviceModel = deviceModel;
    }

    public String getDeviceName() {
        return deviceName;
    }

    public void setDeviceName(String deviceName) {
        this.deviceName = deviceName;
    }

    public String getCountry() {
        return country;
    }

    public void setCountry(String country) {
        this.country = country;
    }

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

    public String getDistributorStoreId() {
        return distributorStoreId;
    }

    public void setDistributorStoreId(String distributorStoreId) {
        this.distributorStoreId = distributorStoreId;
    }

    public String getHomeStoreId() {
        return homeStoreId;
    }

    public void setHomeStoreId(String homeStoreId) {
        this.homeStoreId = homeStoreId;
    }

}
