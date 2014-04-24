package org.onepf.repository.api.responsewriter.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

/**
 * Created by akarimova on 16.04.14.
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "reviewType")
@Entity
@Table(name = "reviews")
public class ReviewEntity extends BaseHashEntity {
    @XmlAttribute(name = "package", required = true)
    @Column(name = "package")
    private String packageName;

    @XmlAttribute(name = "datetime")
    @Column(name = "datetime")
    private String dateTime;

    @XmlAttribute(name = "version")
    @Column(name = "version")
    private String version;

    @XmlAttribute(name = "versionCode", required = true)
    @Column(name = "versionCode")
    private int versionCode;

    @XmlAttribute(name = "device-model", required = true)
    @Column(name = "deviceModel")
    private String deviceModel;

    @XmlAttribute(name = "device-name")
    @Column(name = "deviceName")
    private String deviceName;

    @XmlAttribute(name = "country", required = true)
    @Column(name = "country")
    private String country;

    @XmlAttribute(name = "rating", required = true)
    @Column(name = "rating")
    private float rating;

    @XmlAttribute(name = "user-name")
    @Column(name = "userName")
    private String userName;

    @XmlAttribute(name = "review-url")
    @Column(name = "reviewUrl")
    private String userUrl;

    @XmlAttribute(name = "title")
    @Column(name = "title")
    private String title;

    @XmlAttribute(name = "text")
    @Column(name = "textBody")
    private String body;

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

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public int getVersionCode() {
        return versionCode;
    }

    public void setVersionCode(int versionCode) {
        this.versionCode = versionCode;
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

    public float getRating() {
        return rating;
    }

    public void setRating(float rating) {
        this.rating = rating;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getUserUrl() {
        return userUrl;
    }

    public void setUserUrl(String userUrl) {
        this.userUrl = userUrl;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getBody() {
        return body;
    }

    public void setBody(String body) {
        this.body = body;
    }

    @Id
    private int id;

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
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
