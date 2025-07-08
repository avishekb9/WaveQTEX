// Risk Monitoring System for WaveQTE Network Economics
// Real-time systemic risk assessment and early warning

class RiskMonitor {
    constructor() {
        this.riskComponents = {
            market: 0.34,
            credit: 0.28,
            liquidity: 0.42,
            operational: 0.19
        };
        
        this.indicators = [
            { name: 'VIX Spike', status: 'ok', threshold: 25, current: 18.5 },
            { name: 'Credit Spreads', status: 'warning', threshold: 150, current: 165 },
            { name: 'Liquidity', status: 'ok', threshold: 0.8, current: 0.85 },
            { name: 'Correlation', status: 'critical', threshold: 0.7, current: 0.82 }
        ];
        
        this.historicalData = [];
        this.alertThresholds = {
            low: 0.3,
            moderate: 0.5,
            high: 0.7,
            critical: 0.9
        };
        
        this.init();
    }
    
    init() {
        this.generateHistoricalData();
        this.startRiskUpdates();
        this.setupEventListeners();
    }
    
    generateHistoricalData() {
        // Generate 90 days of historical risk data
        for (let i = 0; i < 90; i++) {
            const date = new Date();
            date.setDate(date.getDate() - (89 - i));
            
            this.historicalData.push({
                date: date,
                market: Math.random() * 0.6 + 0.2,
                credit: Math.random() * 0.5 + 0.15,
                liquidity: Math.random() * 0.7 + 0.25,
                operational: Math.random() * 0.4 + 0.1,
                aggregate: Math.random() * 0.5 + 0.25
            });
        }
    }
    
    startRiskUpdates() {
        setInterval(() => {
            this.updateRiskComponents();
            this.updateIndicators();
            this.updateDisplay();
            this.checkAlerts();
        }, 5000);
    }
    
    updateRiskComponents() {
        // Simulate risk component updates
        Object.keys(this.riskComponents).forEach(component => {
            const change = (Math.random() - 0.5) * 0.05;
            this.riskComponents[component] += change;
            this.riskComponents[component] = Math.max(0, Math.min(1, this.riskComponents[component]));
        });
        
        // Add new data point to historical data
        const newDataPoint = {
            date: new Date(),
            market: this.riskComponents.market,
            credit: this.riskComponents.credit,
            liquidity: this.riskComponents.liquidity,
            operational: this.riskComponents.operational,
            aggregate: this.calculateAggregateRisk()
        };
        
        this.historicalData.push(newDataPoint);
        if (this.historicalData.length > 90) {
            this.historicalData.shift();
        }
    }
    
    updateIndicators() {
        this.indicators.forEach(indicator => {
            // Simulate indicator updates
            const change = (Math.random() - 0.5) * 0.1;
            indicator.current += change;
            indicator.current = Math.max(0, indicator.current);
            
            // Update status based on threshold
            if (indicator.name === 'Liquidity') {
                indicator.status = indicator.current >= indicator.threshold ? 'ok' : 'warning';
            } else {
                if (indicator.current <= indicator.threshold * 0.8) {
                    indicator.status = 'ok';
                } else if (indicator.current <= indicator.threshold) {
                    indicator.status = 'warning';
                } else {
                    indicator.status = 'critical';
                }
            }
        });
    }
    
    calculateAggregateRisk() {
        const weights = {
            market: 0.3,
            credit: 0.25,
            liquidity: 0.25,
            operational: 0.2
        };
        
        return Object.keys(this.riskComponents).reduce((total, component) => {
            return total + this.riskComponents[component] * weights[component];
        }, 0);
    }
    
    updateDisplay() {
        this.updateRiskGauge();
        this.updateRiskComponents();
        this.updateIndicatorsDisplay();
        this.updateRiskLevel();
    }
    
    updateRiskGauge() {
        const aggregateRisk = this.calculateAggregateRisk();
        
        // Update gauge if it exists
        if (window.waveQTEApp && window.waveQTEApp.riskGauge) {
            window.waveQTEApp.riskGauge.setValue(aggregateRisk);
        }
    }
    
    updateRiskComponents() {
        const components = document.querySelectorAll('.component');
        const componentNames = ['market', 'credit', 'liquidity', 'operational'];
        
        components.forEach((component, index) => {
            if (componentNames[index]) {
                const value = this.riskComponents[componentNames[index]];
                const valueSpan = component.querySelector('.component-value');
                const barFill = component.querySelector('.bar-fill');
                
                if (valueSpan) {
                    valueSpan.textContent = value.toFixed(2);
                }
                
                if (barFill) {
                    barFill.style.width = `${value * 100}%`;
                    
                    // Update bar color based on risk level
                    if (value < 0.3) {
                        barFill.style.background = 'linear-gradient(135deg, #4caf50 0%, #8bc34a 100%)';
                    } else if (value < 0.6) {
                        barFill.style.background = 'linear-gradient(135deg, #ffc947 0%, #ff9800 100%)';
                    } else {
                        barFill.style.background = 'linear-gradient(135deg, #ff5722 0%, #f44336 100%)';
                    }
                }
            }
        });
    }
    
    updateIndicatorsDisplay() {
        const indicatorElements = document.querySelectorAll('.indicator');
        
        indicatorElements.forEach((element, index) => {
            if (this.indicators[index]) {
                const indicator = this.indicators[index];
                const icon = element.querySelector('.indicator-icon');
                const status = element.querySelector('.indicator-status');
                
                if (icon) {
                    icon.className = `indicator-icon ${indicator.status}`;
                }
                
                if (status) {
                    status.textContent = this.getStatusText(indicator.status);
                }
            }
        });
    }
    
    getStatusText(status) {
        const statusMap = {
            'ok': 'Normal',
            'warning': 'Elevated',
            'critical': 'High'
        };
        return statusMap[status] || 'Unknown';
    }
    
    updateRiskLevel() {
        const aggregateRisk = this.calculateAggregateRisk();
        const riskLevelElement = document.querySelector('.risk-level');
        
        if (riskLevelElement) {
            let level = 'low';
            let text = 'Low';
            
            if (aggregateRisk >= this.alertThresholds.critical) {
                level = 'critical';
                text = 'Critical';
            } else if (aggregateRisk >= this.alertThresholds.high) {
                level = 'high';
                text = 'High';
            } else if (aggregateRisk >= this.alertThresholds.moderate) {
                level = 'moderate';
                text = 'Moderate';
            }
            
            riskLevelElement.className = `risk-level ${level}`;
            riskLevelElement.textContent = text;
        }
    }
    
    checkAlerts() {
        const aggregateRisk = this.calculateAggregateRisk();
        const alerts = [];
        
        // Check aggregate risk thresholds
        if (aggregateRisk >= this.alertThresholds.critical) {
            alerts.push({
                type: 'critical',
                message: 'CRITICAL: Systemic risk has reached critical levels',
                timestamp: new Date()
            });
        } else if (aggregateRisk >= this.alertThresholds.high) {
            alerts.push({
                type: 'warning',
                message: 'WARNING: High systemic risk detected',
                timestamp: new Date()
            });
        }
        
        // Check individual component thresholds
        Object.keys(this.riskComponents).forEach(component => {
            const value = this.riskComponents[component];
            if (value >= 0.8) {
                alerts.push({
                    type: 'warning',
                    message: `${component.toUpperCase()} risk elevated: ${(value * 100).toFixed(1)}%`,
                    timestamp: new Date()
                });
            }
        });
        
        // Check indicators
        this.indicators.forEach(indicator => {
            if (indicator.status === 'critical') {
                alerts.push({
                    type: 'critical',
                    message: `${indicator.name} has reached critical threshold`,
                    timestamp: new Date()
                });
            }
        });
        
        // Process alerts
        if (alerts.length > 0) {
            this.processAlerts(alerts);
        }
    }
    
    processAlerts(alerts) {
        alerts.forEach(alert => {
            console.log('RISK ALERT:', alert.message);
            
            // Add to alerts list if it exists
            const alertsList = document.getElementById('alerts-list');
            if (alertsList) {
                const alertElement = this.createAlertElement(alert);
                alertsList.insertBefore(alertElement, alertsList.firstChild);
                
                // Remove old alerts (keep only last 5)
                while (alertsList.children.length > 5) {
                    alertsList.removeChild(alertsList.lastChild);
                }
            }
        });
    }
    
    createAlertElement(alert) {
        const alertDiv = document.createElement('div');
        alertDiv.className = `alert-item ${alert.type}`;
        
        const timeString = alert.timestamp.toLocaleTimeString();
        
        alertDiv.innerHTML = `
            <div class="alert-icon">
                <i class="fas ${this.getAlertIcon(alert.type)}"></i>
            </div>
            <div class="alert-content">
                <div class="alert-message">${alert.message}</div>
                <div class="alert-time">${timeString}</div>
            </div>
        `;
        
        return alertDiv;
    }
    
    getAlertIcon(type) {
        const iconMap = {
            'critical': 'fa-exclamation-triangle',
            'warning': 'fa-exclamation-circle',
            'info': 'fa-info-circle'
        };
        return iconMap[type] || 'fa-bell';
    }
    
    setupEventListeners() {
        // Add any interactive elements here
        document.addEventListener('click', (e) => {
            if (e.target.classList.contains('risk-component')) {
                this.showComponentDetails(e.target);
            }
        });
    }
    
    showComponentDetails(element) {
        // Show detailed risk component information
        const componentName = element.getAttribute('data-component');
        if (componentName && this.riskComponents[componentName]) {
            console.log(`${componentName} Risk Details:`, {
                current: this.riskComponents[componentName],
                threshold: this.alertThresholds.moderate,
                trend: 'stable', // Could be calculated from historical data
                contributors: ['Market volatility', 'Cross-correlations', 'Liquidity conditions']
            });
        }
    }
    
    // Public API methods
    getRiskComponents() {
        return { ...this.riskComponents };
    }
    
    getIndicators() {
        return [...this.indicators];
    }
    
    getHistoricalData() {
        return [...this.historicalData];
    }
    
    getAggregateRisk() {
        return this.calculateAggregateRisk();
    }
    
    getRiskLevel() {
        const risk = this.calculateAggregateRisk();
        
        if (risk >= this.alertThresholds.critical) return 'critical';
        if (risk >= this.alertThresholds.high) return 'high';
        if (risk >= this.alertThresholds.moderate) return 'moderate';
        return 'low';
    }
    
    // Stress testing functionality
    runStressTest(scenario, severity = 1) {
        const results = {
            scenario: scenario,
            severity: severity,
            originalRisk: this.calculateAggregateRisk(),
            stressedRisk: null,
            impact: {},
            recommendations: []
        };
        
        // Apply stress scenario
        const stressFactors = this.getStressFactors(scenario, severity);
        const stressedComponents = {};
        
        Object.keys(this.riskComponents).forEach(component => {
            const factor = stressFactors[component] || 1;
            stressedComponents[component] = Math.min(1, this.riskComponents[component] * factor);
        });
        
        // Calculate stressed risk
        const weights = { market: 0.3, credit: 0.25, liquidity: 0.25, operational: 0.2 };
        results.stressedRisk = Object.keys(stressedComponents).reduce((total, component) => {
            return total + stressedComponents[component] * weights[component];
        }, 0);
        
        // Calculate impact
        results.impact = {
            total: results.stressedRisk - results.originalRisk,
            components: {}
        };
        
        Object.keys(stressedComponents).forEach(component => {
            results.impact.components[component] = stressedComponents[component] - this.riskComponents[component];
        });
        
        // Generate recommendations
        if (results.stressedRisk > 0.7) {
            results.recommendations.push('Increase capital buffers');
            results.recommendations.push('Reduce leverage ratios');
            results.recommendations.push('Enhance liquidity management');
        }
        
        return results;
    }
    
    getStressFactors(scenario, severity) {
        const baseFactors = {
            'market_crash': { market: 2.0, credit: 1.5, liquidity: 1.8, operational: 1.2 },
            'credit_crisis': { market: 1.3, credit: 2.5, liquidity: 1.6, operational: 1.4 },
            'liquidity_crisis': { market: 1.4, credit: 1.8, liquidity: 3.0, operational: 1.3 },
            'operational_failure': { market: 1.2, credit: 1.3, liquidity: 1.4, operational: 2.8 }
        };
        
        const factors = baseFactors[scenario] || { market: 1.5, credit: 1.5, liquidity: 1.5, operational: 1.5 };
        
        // Apply severity multiplier
        Object.keys(factors).forEach(component => {
            factors[component] = 1 + (factors[component] - 1) * severity;
        });
        
        return factors;
    }
    
    destroy() {
        // Cleanup method
        if (this.updateInterval) {
            clearInterval(this.updateInterval);
        }
    }
}

// Early Warning System
class EarlyWarningSystem {
    constructor() {
        this.indicators = [];
        this.thresholds = {};
        this.predictions = [];
        this.isActive = true;
        
        this.init();
    }
    
    init() {
        this.setupIndicators();
        this.startMonitoring();
    }
    
    setupIndicators() {
        this.indicators = [
            {
                name: 'correlation_spike',
                description: 'Sudden increase in cross-market correlations',
                threshold: 0.8,
                current: 0.65,
                weight: 0.3
            },
            {
                name: 'volatility_clustering',
                description: 'Increased volatility clustering patterns',
                threshold: 0.7,
                current: 0.45,
                weight: 0.25
            },
            {
                name: 'liquidity_deterioration',
                description: 'Declining market liquidity conditions',
                threshold: 0.6,
                current: 0.35,
                weight: 0.25
            },
            {
                name: 'network_fragility',
                description: 'Increased network fragility measures',
                threshold: 0.75,
                current: 0.52,
                weight: 0.2
            }
        ];
    }
    
    startMonitoring() {
        setInterval(() => {
            this.updateIndicators();
            this.generatePredictions();
            this.checkWarnings();
        }, 10000); // Check every 10 seconds
    }
    
    updateIndicators() {
        this.indicators.forEach(indicator => {
            // Simulate indicator updates
            const change = (Math.random() - 0.5) * 0.05;
            indicator.current += change;
            indicator.current = Math.max(0, Math.min(1, indicator.current));
        });
    }
    
    generatePredictions() {
        const warningScore = this.calculateWarningScore();
        
        const prediction = {
            timestamp: new Date(),
            warningScore: warningScore,
            crisisProb: this.calculateCrisisProbability(warningScore),
            timeHorizon: this.estimateTimeHorizon(warningScore),
            confidence: this.calculateConfidence(warningScore)
        };
        
        this.predictions.push(prediction);
        
        // Keep only last 100 predictions
        if (this.predictions.length > 100) {
            this.predictions.shift();
        }
    }
    
    calculateWarningScore() {
        return this.indicators.reduce((score, indicator) => {
            const normalized = indicator.current / indicator.threshold;
            return score + normalized * indicator.weight;
        }, 0);
    }
    
    calculateCrisisProbability(warningScore) {
        // Simple logistic function for crisis probability
        return 1 / (1 + Math.exp(-5 * (warningScore - 0.6)));
    }
    
    estimateTimeHorizon(warningScore) {
        // Estimate time until potential crisis (in days)
        if (warningScore > 0.8) return 30;
        if (warningScore > 0.6) return 60;
        if (warningScore > 0.4) return 90;
        return 120;
    }
    
    calculateConfidence(warningScore) {
        // Confidence in prediction (0-1)
        return Math.min(1, warningScore * 1.2);
    }
    
    checkWarnings() {
        const latestPrediction = this.predictions[this.predictions.length - 1];
        
        if (latestPrediction && latestPrediction.crisisProb > 0.7) {
            this.triggerEarlyWarning(latestPrediction);
        }
    }
    
    triggerEarlyWarning(prediction) {
        const warning = {
            type: 'early_warning',
            severity: prediction.crisisProb > 0.9 ? 'critical' : 'high',
            message: `Early Warning: ${(prediction.crisisProb * 100).toFixed(1)}% crisis probability within ${prediction.timeHorizon} days`,
            timestamp: new Date(),
            details: {
                crisisProb: prediction.crisisProb,
                timeHorizon: prediction.timeHorizon,
                confidence: prediction.confidence,
                indicators: this.indicators.filter(i => i.current > i.threshold * 0.8)
            }
        };
        
        console.log('EARLY WARNING TRIGGERED:', warning);
        
        // Send to main risk monitor
        if (window.riskMonitor) {
            window.riskMonitor.processAlerts([warning]);
        }
    }
    
    getPredictions() {
        return [...this.predictions];
    }
    
    getIndicators() {
        return [...this.indicators];
    }
    
    getLatestPrediction() {
        return this.predictions[this.predictions.length - 1];
    }
}

// Initialize risk monitoring systems
let riskMonitor = null;
let earlyWarningSystem = null;

document.addEventListener('DOMContentLoaded', () => {
    riskMonitor = new RiskMonitor();
    earlyWarningSystem = new EarlyWarningSystem();
    
    // Make available globally
    window.riskMonitor = riskMonitor;
    window.earlyWarningSystem = earlyWarningSystem;
});

// Export classes
window.RiskMonitor = RiskMonitor;
window.EarlyWarningSystem = EarlyWarningSystem;