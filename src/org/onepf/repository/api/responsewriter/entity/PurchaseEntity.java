package org.onepf.repository.api.responsewriter.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * Created by akarimova on 16.04.14.
 */
@Entity
@Table(name = "purchases")
public class PurchaseEntity extends BaseEntity{
    @Column(name = "package")
    private String packageName;

    @Column(name = "datetime")
    private String dateTime;

    @javax.persistence.Id
    private int id;

    @Column(name = "version")
    private String version;

    @Column(name = "versionCode")
    private int build;

    @Column(name = "lastUpdate")
    private String lastUpdate;

    @Column(name = "deviceModel")
    private String deviceModel;

    @Column(name = "deviceName")
    private String deviceName;

    @Column(name = "country")
    private String country;

    @Column(name = "userPrice")
    private String userPrice;

    @Column(name = "userCurrency")
    private String userCurrency;

    @Column(name = "innerPrice")
    private String innerPrice;

    @Column(name = "innerCurrency")
    private String innerCurrency;

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

}
