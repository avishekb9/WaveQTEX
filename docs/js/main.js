// WaveQTE Network Economics - Main JavaScript
// Futuristic Web Interface Controller

class WaveQTEApp {
    constructor() {
        this.currentSection = 'dashboard';
        this.theme = 'dark';
        this.isLoading = true;
        this.animationFrame = null;
        this.websocket = null;
        this.dataUpdateInterval = null;
        
        this.init();
    }
    
    init() {
        this.setupEventListeners();
        this.initializeAOS();
        this.setupTheme();
        this.initializeWebSocket();
        this.startDataUpdates();
        this.hideLoadingScreen();
    }
    
    setupEventListeners() {
        // Navigation
        document.querySelectorAll('.nav-link').forEach(link => {
            link.addEventListener('click', (e) => this.handleNavigation(e));
        });
        
        // Theme toggle
        const themeToggle = document.getElementById('theme-toggle');
        if (themeToggle) {
            themeToggle.addEventListener('click', () => this.toggleTheme());
        }
        
        // Fullscreen toggle
        const fullscreenToggle = document.getElementById('fullscreen-toggle');
        if (fullscreenToggle) {
            fullscreenToggle.addEventListener('click', () => this.toggleFullscreen());
        }
        
        // Hamburger menu
        const hamburger = document.getElementById('hamburger');
        if (hamburger) {
            hamburger.addEventListener('click', () => this.toggleMobileMenu());
        }
        
        // Scale selector
        const scaleSelector = document.getElementById('scale-selector');
        if (scaleSelector) {
            scaleSelector.addEventListener('change', (e) => this.handleScaleChange(e));
        }
        
        // Play/pause button
        const playPause = document.getElementById('play-pause');
        if (playPause) {
            playPause.addEventListener('click', () => this.toggleAnimation());
        }
        
        // Scale buttons
        document.querySelectorAll('.scale-btn').forEach(btn => {
            btn.addEventListener('click', (e) => this.handleScaleButtonClick(e));
        });
        
        // Edge threshold slider
        const edgeThreshold = document.getElementById('edge-threshold');
        if (edgeThreshold) {
            edgeThreshold.addEventListener('input', (e) => this.handleThresholdChange(e));
        }
        
        // Layout selector
        const layoutSelector = document.getElementById('layout-selector');
        if (layoutSelector) {
            layoutSelector.addEventListener('change', (e) => this.handleLayoutChange(e));
        }
        
        // Node size selector
        const nodeSize = document.getElementById('node-size');
        if (nodeSize) {
            nodeSize.addEventListener('change', (e) => this.handleNodeSizeChange(e));
        }
        
        // Action buttons
        document.querySelectorAll('.action-btn').forEach(btn => {
            btn.addEventListener('click', (e) => this.handleActionButton(e));
        });
        
        // Keyboard shortcuts
        document.addEventListener('keydown', (e) => this.handleKeyboard(e));
        
        // Window resize
        window.addEventListener('resize', () => this.handleResize());
        
        // Visibility change (tab switching)
        document.addEventListener('visibilitychange', () => this.handleVisibilityChange());
    }
    
    initializeAOS() {
        AOS.init({
            duration: 800,
            easing: 'ease-out-cubic',
            once: true,
            offset: 50
        });
    }
    
    setupTheme() {
        const savedTheme = localStorage.getItem('waveqte-theme');
        if (savedTheme) {
            this.theme = savedTheme;
            document.documentElement.setAttribute('data-theme', this.theme);
        }
        this.updateThemeIcon();
    }
    
    toggleTheme() {
        this.theme = this.theme === 'dark' ? 'light' : 'dark';
        document.documentElement.setAttribute('data-theme', this.theme);
        localStorage.setItem('waveqte-theme', this.theme);
        this.updateThemeIcon();
        
        // Animate theme transition
        document.body.style.transition = 'background-color 0.3s ease, color 0.3s ease';
        setTimeout(() => {
            document.body.style.transition = '';
        }, 300);
    }
    
    updateThemeIcon() {
        const themeIcon = document.querySelector('#theme-toggle i');
        if (themeIcon) {
            themeIcon.className = this.theme === 'dark' ? 'fas fa-sun' : 'fas fa-moon';
        }
    }
    
    toggleFullscreen() {
        if (!document.fullscreenElement) {
            document.documentElement.requestFullscreen().catch(err => {
                console.log(`Error attempting to enable fullscreen: ${err.message}`);
            });
        } else {
            document.exitFullscreen();
        }
        
        // Update icon
        const fullscreenIcon = document.querySelector('#fullscreen-toggle i');
        if (fullscreenIcon) {
            fullscreenIcon.className = document.fullscreenElement ? 
                'fas fa-compress' : 'fas fa-expand';
        }
    }
    
    toggleMobileMenu() {
        const navMenu = document.getElementById('nav-menu');
        const hamburger = document.getElementById('hamburger');
        
        if (navMenu && hamburger) {
            navMenu.classList.toggle('active');
            hamburger.classList.toggle('active');
        }
    }
    
    handleNavigation(e) {
        e.preventDefault();
        
        const link = e.currentTarget;
        const targetSection = link.getAttribute('data-section');
        
        if (targetSection && targetSection !== this.currentSection) {
            this.switchSection(targetSection);
        }
    }
    
    switchSection(sectionId) {
        // Hide current section
        const currentElement = document.getElementById(this.currentSection);
        if (currentElement) {
            currentElement.classList.remove('active');
        }
        
        // Show new section
        const newElement = document.getElementById(sectionId);
        if (newElement) {
            newElement.classList.add('active');
        }
        
        // Update navigation
        document.querySelectorAll('.nav-link').forEach(link => {
            link.classList.remove('active');
            if (link.getAttribute('data-section') === sectionId) {
                link.classList.add('active');
            }
        });
        
        this.currentSection = sectionId;
        
        // Initialize section-specific functionality
        this.initializeSection(sectionId);
        
        // Update URL without reload
        window.history.pushState({ section: sectionId }, '', `#${sectionId}`);
    }
    
    initializeSection(sectionId) {
        switch (sectionId) {
            case 'dashboard':
                this.initializeDashboard();
                break;
            case 'network':
                this.initializeNetwork3D();
                break;
            case 'agents':
                this.initializeAgents();
                break;
            case 'risk':
                this.initializeRiskMonitor();
                break;
            case 'analysis':
                this.initializeAnalysis();
                break;
        }
    }
    
    initializeDashboard() {
        // Initialize main network canvas
        this.initializeNetworkCanvas();
        
        // Initialize mini charts
        this.initializeCorrelationHeatmap();
        this.initializeRiskTimeline();
        this.initializeAgentActivity();
        this.initializeMarketStatus();
        this.initializeAlertsList();
        
        // Start real-time updates
        this.startDashboardUpdates();
    }
    
    initializeNetworkCanvas() {
        const canvas = document.getElementById('network-canvas');
        if (!canvas) return;
        
        const ctx = canvas.getContext('2d');
        canvas.width = canvas.offsetWidth * window.devicePixelRatio;
        canvas.height = canvas.offsetHeight * window.devicePixelRatio;
        ctx.scale(window.devicePixelRatio, window.devicePixelRatio);
        
        // Create animated network visualization
        this.networkAnimation = new NetworkAnimation(canvas, ctx);
        this.networkAnimation.start();
    }
    
    initializeCorrelationHeatmap() {
        const container = document.getElementById('correlation-heatmap');
        if (!container) return;
        
        // Generate sample correlation matrix
        const markets = ['US', 'EU', 'JP', 'CN', 'IN', 'BR'];
        const correlationData = this.generateCorrelationMatrix(markets);
        
        // Create D3 heatmap
        this.correlationHeatmap = new CorrelationHeatmap(container, correlationData);
    }
    
    initializeRiskTimeline() {
        const canvas = document.getElementById('risk-timeline');
        if (!canvas) return;
        
        // Create Chart.js timeline
        this.riskChart = new Chart(canvas, {
            type: 'line',
            data: {
                labels: this.generateTimeLabels(30),
                datasets: [{
                    label: 'Systemic Risk Index',
                    data: this.generateRiskData(30),
                    borderColor: '#00e5ff',
                    backgroundColor: 'rgba(0, 229, 255, 0.1)',
                    borderWidth: 2,
                    fill: true,
                    tension: 0.4
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    legend: {
                        display: false
                    }
                },
                scales: {
                    x: {
                        display: false
                    },
                    y: {
                        display: false
                    }
                },
                elements: {
                    point: {
                        radius: 0
                    }
                }
            }
        });
    }
    
    initializeAgentActivity() {
        const container = document.getElementById('agent-activity');
        if (!container) return;
        
        const activities = [
            { agent: 'HFT-001', action: 'Market Order', time: '10:32:45', status: 'success' },
            { agent: 'MM-015', action: 'Liquidity Provision', time: '10:32:44', status: 'success' },
            { agent: 'INST-007', action: 'Portfolio Rebalance', time: '10:32:40', status: 'pending' },
            { agent: 'REG-002', action: 'Risk Assessment', time: '10:32:35', status: 'warning' },
            { agent: 'HFT-023', action: 'Arbitrage', time: '10:32:30', status: 'success' }
        ];
        
        this.renderAgentActivity(container, activities);
    }
    
    initializeMarketStatus() {
        const container = document.getElementById('market-status');
        if (!container) return;
        
        const markets = [
            { name: 'US', status: 'open', change: '+0.8%' },
            { name: 'EU', status: 'open', change: '+0.3%' },
            { name: 'JP', status: 'closed', change: '-0.2%' },
            { name: 'CN', status: 'open', change: '+1.2%' },
            { name: 'IN', status: 'open', change: '+0.9%' },
            { name: 'BR', status: 'closed', change: '+0.1%' }
        ];
        
        this.renderMarketStatus(container, markets);
    }
    
    initializeAlertsList() {
        const container = document.getElementById('alerts-list');
        if (!container) return;
        
        const alerts = [
            { type: 'warning', message: 'Credit spreads elevated in EU markets', time: '2 min ago' },
            { type: 'info', message: 'New agent cluster detected in Asia', time: '5 min ago' },
            { type: 'success', message: 'Risk index decreased by 5%', time: '8 min ago' },
            { type: 'warning', message: 'High correlation detected between US-EU', time: '12 min ago' }
        ];
        
        this.renderAlertsList(container, alerts);
    }
    
    initializeNetwork3D() {
        const container = document.getElementById('network-3d');
        if (!container || !window.THREE) return;
        
        try {
            this.network3D = new Network3D(container);
            this.network3D.initialize();
        } catch (error) {
            console.error('Failed to initialize 3D network:', error);
            container.innerHTML = '<div class="error-message">3D visualization not available</div>';
        }
    }
    
    initializeAgents() {
        this.initializeAgentNetwork();
        this.initializeAgentPerformance();
        this.updateProtocolStatus();
    }
    
    initializeAgentNetwork() {
        const container = document.getElementById('agent-network');
        if (!container) return;
        
        // Create agent communication network visualization
        this.agentNetworkViz = new AgentNetworkVisualization(container);
    }
    
    initializeAgentPerformance() {
        const canvas = document.getElementById('agent-performance');
        if (!canvas) return;
        
        this.agentPerformanceChart = new Chart(canvas, {
            type: 'line',
            data: {
                labels: this.generateTimeLabels(24),
                datasets: [
                    {
                        label: 'HFT Agents',
                        data: this.generatePerformanceData(24, 0.95),
                        borderColor: '#00e5ff',
                        backgroundColor: 'rgba(0, 229, 255, 0.1)'
                    },
                    {
                        label: 'Market Makers',
                        data: this.generatePerformanceData(24, 0.88),
                        borderColor: '#64ffda',
                        backgroundColor: 'rgba(100, 255, 218, 0.1)'
                    },
                    {
                        label: 'Institutional',
                        data: this.generatePerformanceData(24, 0.92),
                        borderColor: '#1de9b6',
                        backgroundColor: 'rgba(29, 233, 182, 0.1)'
                    }
                ]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    legend: {
                        position: 'top'
                    }
                },
                scales: {
                    y: {
                        beginAtZero: true,
                        max: 1
                    }
                }
            }
        });
    }
    
    initializeRiskMonitor() {
        this.initializeRiskGauge();
        this.initializeRiskEvolution();
        this.updateRiskComponents();
        this.updateEarlyWarningIndicators();
    }
    
    initializeRiskGauge() {
        const canvas = document.getElementById('risk-gauge');
        if (!canvas) return;
        
        this.riskGauge = new RiskGauge(canvas);
        this.riskGauge.setValue(0.34);
    }
    
    initializeRiskEvolution() {
        const canvas = document.getElementById('risk-evolution');
        if (!canvas) return;
        
        this.riskEvolutionChart = new Chart(canvas, {
            type: 'line',
            data: {
                labels: this.generateTimeLabels(90),
                datasets: [
                    {
                        label: 'Market Risk',
                        data: this.generateRiskData(90, 0.3),
                        borderColor: '#ff5722',
                        backgroundColor: 'rgba(255, 87, 34, 0.1)'
                    },
                    {
                        label: 'Credit Risk',
                        data: this.generateRiskData(90, 0.25),
                        borderColor: '#ffc947',
                        backgroundColor: 'rgba(255, 201, 71, 0.1)'
                    },
                    {
                        label: 'Liquidity Risk',
                        data: this.generateRiskData(90, 0.4),
                        borderColor: '#00e5ff',
                        backgroundColor: 'rgba(0, 229, 255, 0.1)'
                    },
                    {
                        label: 'Operational Risk',
                        data: this.generateRiskData(90, 0.15),
                        borderColor: '#64ffda',
                        backgroundColor: 'rgba(100, 255, 218, 0.1)'
                    }
                ]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    legend: {
                        position: 'top'
                    }
                },
                scales: {
                    y: {
                        beginAtZero: true,
                        max: 1
                    }
                }
            }
        });
    }
    
    initializeAnalysis() {
        this.setupAnalysisTools();
        this.clearAnalysisOutput();
    }
    
    setupAnalysisTools() {
        // Initialize analysis tool configurations
        this.analysisConfig = {
            wavelet: {
                type: 'daubechies',
                levels: 6
            },
            transferEntropy: {
                lag: 3,
                quantile: 'both'
            },
            stressTesting: {
                scenario: 'market_crash',
                severity: 3
            }
        };
    }
    
    clearAnalysisOutput() {
        const output = document.getElementById('analysis-output');
        if (output) {
            output.innerHTML = '<p>Select an analysis tool to view results...</p>';
        }
    }
    
    // Event Handlers
    handleScaleChange(e) {
        const scale = e.target.value;
        console.log('Scale changed to:', scale);
        
        // Update visualizations based on scale
        if (this.networkAnimation) {
            this.networkAnimation.setScale(scale);
        }
    }
    
    toggleAnimation() {
        const btn = document.getElementById('play-pause');
        const icon = btn.querySelector('i');
        
        if (this.networkAnimation) {
            if (this.networkAnimation.isPlaying) {
                this.networkAnimation.pause();
                icon.className = 'fas fa-play';
            } else {
                this.networkAnimation.play();
                icon.className = 'fas fa-pause';
            }
        }
    }
    
    handleScaleButtonClick(e) {
        // Remove active class from all buttons
        document.querySelectorAll('.scale-btn').forEach(btn => {
            btn.classList.remove('active');
        });
        
        // Add active class to clicked button
        e.target.classList.add('active');
        
        const scale = e.target.getAttribute('data-scale');
        console.log('3D Network scale changed to:', scale);
        
        if (this.network3D) {
            this.network3D.setScale(parseInt(scale));
        }
    }
    
    handleThresholdChange(e) {
        const threshold = parseFloat(e.target.value);
        document.getElementById('threshold-value').textContent = threshold.toFixed(2);
        
        if (this.network3D) {
            this.network3D.setThreshold(threshold);
        }
    }
    
    handleLayoutChange(e) {
        const layout = e.target.value;
        console.log('Layout changed to:', layout);
        
        if (this.network3D) {
            this.network3D.setLayout(layout);
        }
    }
    
    handleNodeSizeChange(e) {
        const sizeMetric = e.target.value;
        console.log('Node size metric changed to:', sizeMetric);
        
        if (this.network3D) {
            this.network3D.setNodeSize(sizeMetric);
        }
    }
    
    handleActionButton(e) {
        const button = e.target;
        const panel = button.closest('.tool-panel');
        const toolType = panel.querySelector('h3').textContent.toLowerCase().replace(' ', '_');
        
        this.runAnalysis(toolType, button);
    }
    
    handleKeyboard(e) {
        // Keyboard shortcuts
        if (e.ctrlKey || e.metaKey) {
            switch (e.key) {
                case '1':
                    e.preventDefault();
                    this.switchSection('dashboard');
                    break;
                case '2':
                    e.preventDefault();
                    this.switchSection('network');
                    break;
                case '3':
                    e.preventDefault();
                    this.switchSection('agents');
                    break;
                case '4':
                    e.preventDefault();
                    this.switchSection('risk');
                    break;
                case '5':
                    e.preventDefault();
                    this.switchSection('analysis');
                    break;
                case 't':
                    e.preventDefault();
                    this.toggleTheme();
                    break;
                case 'f':
                    e.preventDefault();
                    this.toggleFullscreen();
                    break;
            }
        }
        
        // Space bar to toggle animation
        if (e.key === ' ' && this.currentSection === 'dashboard') {
            e.preventDefault();
            this.toggleAnimation();
        }
    }
    
    handleResize() {
        // Update canvas sizes
        if (this.networkAnimation) {
            this.networkAnimation.handleResize();
        }
        
        if (this.network3D) {
            this.network3D.handleResize();
        }
        
        // Update charts
        if (this.riskChart) {
            this.riskChart.resize();
        }
        
        if (this.agentPerformanceChart) {
            this.agentPerformanceChart.resize();
        }
        
        if (this.riskEvolutionChart) {
            this.riskEvolutionChart.resize();
        }
    }
    
    handleVisibilityChange() {
        if (document.hidden) {
            // Pause animations when tab is not visible
            if (this.networkAnimation && this.networkAnimation.isPlaying) {
                this.networkAnimation.pause();
                this.wasPlaying = true;
            }
        } else {
            // Resume animations when tab becomes visible
            if (this.networkAnimation && this.wasPlaying) {
                this.networkAnimation.play();
                this.wasPlaying = false;
            }
        }
    }
    
    // WebSocket for real-time data
    initializeWebSocket() {
        // In a real implementation, this would connect to a WebSocket server
        // For now, we'll simulate with periodic updates
        console.log('WebSocket connection simulated');
    }
    
    startDataUpdates() {
        this.dataUpdateInterval = setInterval(() => {
            this.updateMetrics();
            this.updateNetworkData();
            this.updateRiskData();
            this.updateAgentData();
        }, 5000); // Update every 5 seconds
    }
    
    updateMetrics() {
        // Update dashboard metrics with new data
        const sriValue = document.getElementById('sri-value');
        const densityValue = document.getElementById('density-value');
        const agentsValue = document.getElementById('agents-value');
        const teValue = document.getElementById('te-value');
        
        if (sriValue) {
            const newSRI = (Math.random() * 0.1 + 0.3).toFixed(3);
            this.animateValueChange(sriValue, newSRI);
        }
        
        if (densityValue) {
            const newDensity = (Math.random() * 0.2 + 0.6).toFixed(3);
            this.animateValueChange(densityValue, newDensity);
        }
        
        if (agentsValue) {
            const newAgents = Math.floor(Math.random() * 100 + 1200).toLocaleString();
            this.animateValueChange(agentsValue, newAgents);
        }
        
        if (teValue) {
            const newTE = (Math.random() * 0.2 + 0.7).toFixed(3);
            this.animateValueChange(teValue, newTE);
        }
    }
    
    updateNetworkData() {
        if (this.networkAnimation) {
            this.networkAnimation.updateData();
        }
        
        if (this.network3D) {
            this.network3D.updateData();
        }
    }
    
    updateRiskData() {
        if (this.riskGauge) {
            const newRisk = Math.random() * 0.3 + 0.2;
            this.riskGauge.setValue(newRisk);
        }
        
        // Update risk components
        this.updateRiskComponents();
    }
    
    updateAgentData() {
        // Update agent counts and metrics
        this.updateProtocolStatus();
    }
    
    // Utility functions
    animateValueChange(element, newValue) {
        element.style.transform = 'scale(1.1)';
        element.style.color = '#00e5ff';
        
        setTimeout(() => {
            element.textContent = newValue;
            element.style.transform = 'scale(1)';
            element.style.color = '';
        }, 150);
    }
    
    generateCorrelationMatrix(markets) {
        const matrix = [];
        for (let i = 0; i < markets.length; i++) {
            const row = [];
            for (let j = 0; j < markets.length; j++) {
                if (i === j) {
                    row.push(1);
                } else {
                    row.push(Math.random() * 0.8 + 0.1);
                }
            }
            matrix.push(row);
        }
        return { markets, matrix };
    }
    
    generateTimeLabels(count) {
        const labels = [];
        const now = new Date();
        for (let i = count - 1; i >= 0; i--) {
            const date = new Date(now.getTime() - i * 24 * 60 * 60 * 1000);
            labels.push(date.toLocaleDateString());
        }
        return labels;
    }
    
    generateRiskData(count, base = 0.3) {
        const data = [];
        let current = base;
        for (let i = 0; i < count; i++) {
            current += (Math.random() - 0.5) * 0.1;
            current = Math.max(0, Math.min(1, current));
            data.push(current);
        }
        return data;
    }
    
    generatePerformanceData(count, base = 0.9) {
        const data = [];
        let current = base;
        for (let i = 0; i < count; i++) {
            current += (Math.random() - 0.5) * 0.05;
            current = Math.max(0, Math.min(1, current));
            data.push(current);
        }
        return data;
    }
    
    renderAgentActivity(container, activities) {
        const html = activities.map(activity => `
            <div class="activity-item">
                <div class="activity-info">
                    <span class="agent-id">${activity.agent}</span>
                    <span class="action">${activity.action}</span>
                </div>
                <div class="activity-meta">
                    <span class="time">${activity.time}</span>
                    <span class="status ${activity.status}">${activity.status}</span>
                </div>
            </div>
        `).join('');
        
        container.innerHTML = html;
    }
    
    renderMarketStatus(container, markets) {
        const html = markets.map(market => `
            <div class="market-item">
                <div class="market-name">${market.name}</div>
                <div class="market-status ${market.status}">${market.status}</div>
                <div class="market-change ${market.change.startsWith('+') ? 'positive' : 'negative'}">
                    ${market.change}
                </div>
            </div>
        `).join('');
        
        container.innerHTML = html;
    }
    
    renderAlertsList(container, alerts) {
        const html = alerts.map(alert => `
            <div class="alert-item ${alert.type}">
                <div class="alert-icon">
                    <i class="fas ${this.getAlertIcon(alert.type)}"></i>
                </div>
                <div class="alert-content">
                    <div class="alert-message">${alert.message}</div>
                    <div class="alert-time">${alert.time}</div>
                </div>
            </div>
        `).join('');
        
        container.innerHTML = html;
    }
    
    getAlertIcon(type) {
        switch (type) {
            case 'warning': return 'fa-exclamation-triangle';
            case 'error': return 'fa-times-circle';
            case 'success': return 'fa-check-circle';
            case 'info': return 'fa-info-circle';
            default: return 'fa-bell';
        }
    }
    
    updateRiskComponents() {
        const components = [
            { name: 'Market Risk', value: Math.random() * 0.5 + 0.2 },
            { name: 'Credit Risk', value: Math.random() * 0.4 + 0.1 },
            { name: 'Liquidity Risk', value: Math.random() * 0.6 + 0.3 },
            { name: 'Operational Risk', value: Math.random() * 0.3 + 0.1 }
        ];
        
        document.querySelectorAll('.component').forEach((component, index) => {
            if (components[index]) {
                const valueSpan = component.querySelector('.component-value');
                const barFill = component.querySelector('.bar-fill');
                
                if (valueSpan && barFill) {
                    valueSpan.textContent = components[index].value.toFixed(2);
                    barFill.style.width = `${components[index].value * 100}%`;
                }
            }
        });
    }
    
    updateEarlyWarningIndicators() {
        const indicators = document.querySelectorAll('.indicator');
        indicators.forEach(indicator => {
            const icon = indicator.querySelector('.indicator-icon');
            const status = indicator.querySelector('.indicator-status');
            
            if (Math.random() < 0.1) { // 10% chance to change status
                const statuses = ['ok', 'warning', 'critical'];
                const statusTexts = ['Normal', 'Elevated', 'High'];
                const newStatus = statuses[Math.floor(Math.random() * statuses.length)];
                const newStatusText = statusTexts[statuses.indexOf(newStatus)];
                
                icon.className = `indicator-icon ${newStatus}`;
                status.textContent = newStatusText;
            }
        });
    }
    
    updateProtocolStatus() {
        const messagesPerSec = Math.floor(Math.random() * 500 + 1000);
        const bandwidth = Math.floor(Math.random() * 30 + 60);
        const latency = (Math.random() * 2 + 1).toFixed(1);
        const successRate = (Math.random() * 2 + 98).toFixed(1);
        
        const statusItems = document.querySelectorAll('.status-item .status-value');
        if (statusItems.length >= 4) {
            statusItems[0].textContent = messagesPerSec.toLocaleString();
            statusItems[1].textContent = `${bandwidth}%`;
            statusItems[2].textContent = `${latency}ms`;
            statusItems[3].textContent = `${successRate}%`;
        }
    }
    
    runAnalysis(toolType, button) {
        const output = document.getElementById('analysis-output');
        if (!output) return;
        
        // Show loading state
        button.disabled = true;
        button.textContent = 'Running...';
        output.innerHTML = '<div class="loading">Running analysis...</div>';
        
        // Simulate analysis
        setTimeout(() => {
            let result = '';
            
            switch (toolType) {
                case 'wavelet_analysis':
                    result = this.generateWaveletAnalysisResult();
                    break;
                case 'transfer_entropy':
                    result = this.generateTransferEntropyResult();
                    break;
                case 'stress_testing':
                    result = this.generateStressTestResult();
                    break;
                default:
                    result = 'Analysis completed successfully.';
            }
            
            output.innerHTML = result;
            button.disabled = false;
            button.textContent = button.textContent.replace('Running...', '').trim();
        }, 2000);
    }
    
    generateWaveletAnalysisResult() {
        return `
            <h4>Wavelet Decomposition Results</h4>
            <p><strong>Wavelet:</strong> Daubechies-4</p>
            <p><strong>Decomposition Levels:</strong> 6</p>
            <p><strong>Energy Distribution:</strong></p>
            <ul>
                <li>Scale 1 (1-2 days): 23.4%</li>
                <li>Scale 2 (2-4 days): 31.2%</li>
                <li>Scale 3 (4-8 days): 21.8%</li>
                <li>Scale 4 (8-16 days): 14.5%</li>
                <li>Scale 5 (16-32 days): 6.7%</li>
                <li>Scale 6 (32+ days): 2.4%</li>
            </ul>
            <p><strong>Dominant Frequency:</strong> 2-4 day cycle</p>
            <p><strong>Coherence Score:</strong> 0.73</p>
        `;
    }
    
    generateTransferEntropyResult() {
        return `
            <h4>Transfer Entropy Analysis</h4>
            <p><strong>Lag Order:</strong> 3</p>
            <p><strong>Quantiles:</strong> 5% and 95%</p>
            <p><strong>Significant Connections:</strong></p>
            <ul>
                <li>US → EU: TE = 0.234 (p < 0.001)</li>
                <li>EU → JP: TE = 0.187 (p < 0.01)</li>
                <li>CN → IN: TE = 0.156 (p < 0.05)</li>
                <li>US → CN: TE = 0.143 (p < 0.05)</li>
            </ul>
            <p><strong>Network Density:</strong> 0.68</p>
            <p><strong>Clustering Coefficient:</strong> 0.42</p>
        `;
    }
    
    generateStressTestResult() {
        return `
            <h4>Stress Test Results</h4>
            <p><strong>Scenario:</strong> Market Crash (-20% shock)</p>
            <p><strong>Severity Level:</strong> 3/5</p>
            <p><strong>Impact Analysis:</strong></p>
            <ul>
                <li>Portfolio Losses: 12.7%</li>
                <li>Liquidity Shortfall: $2.1B</li>
                <li>Failing Institutions: 3</li>
                <li>Contagion Spread: 67% of network</li>
            </ul>
            <p><strong>Recovery Time:</strong> 8-12 days</p>
            <p><strong>Recommendation:</strong> Increase capital buffers by 15%</p>
        `;
    }
    
    hideLoadingScreen() {
        setTimeout(() => {
            const loadingScreen = document.getElementById('loading-screen');
            if (loadingScreen) {
                loadingScreen.classList.add('hidden');
                this.isLoading = false;
            }
        }, 2000);
    }
    
    startDashboardUpdates() {
        if (this.currentSection === 'dashboard') {
            // Start specific dashboard animations and updates
            console.log('Dashboard updates started');
        }
    }
    
    destroy() {
        // Cleanup function
        if (this.dataUpdateInterval) {
            clearInterval(this.dataUpdateInterval);
        }
        
        if (this.animationFrame) {
            cancelAnimationFrame(this.animationFrame);
        }
        
        if (this.websocket) {
            this.websocket.close();
        }
        
        // Destroy chart instances
        if (this.riskChart) this.riskChart.destroy();
        if (this.agentPerformanceChart) this.agentPerformanceChart.destroy();
        if (this.riskEvolutionChart) this.riskEvolutionChart.destroy();
        
        // Destroy 3D network
        if (this.network3D) this.network3D.destroy();
        
        // Destroy animations
        if (this.networkAnimation) this.networkAnimation.destroy();
    }
}

// Initialize application when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    window.waveQTEApp = new WaveQTEApp();
});

// Handle browser back/forward buttons
window.addEventListener('popstate', (e) => {
    if (e.state && e.state.section) {
        window.waveQTEApp.switchSection(e.state.section);
    }
});

// Handle page unload
window.addEventListener('beforeunload', () => {
    if (window.waveQTEApp) {
        window.waveQTEApp.destroy();
    }
});