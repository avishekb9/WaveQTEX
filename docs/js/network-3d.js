// 3D Network Visualization for WaveQTE
// Real-time interactive network visualization using Three.js

class Network3D {
    constructor(container) {
        this.container = container;
        this.scene = null;
        this.camera = null;
        this.renderer = null;
        this.controls = null;
        
        this.nodes = [];
        this.edges = [];
        this.nodeObjects = [];
        this.edgeObjects = [];
        
        this.animationFrame = null;
        this.isInitialized = false;
        
        // Configuration
        this.config = {
            nodeCount: 24,
            nodeSize: {
                min: 0.5,
                max: 2.0
            },
            edgeOpacity: {
                min: 0.1,
                max: 0.8
            },
            colors: {
                developed: 0x00e5ff,
                emerging: 0x64ffda,
                central: 0xffc947,
                edge: 0x1de9b6
            },
            layout: 'force',
            scale: 1,
            threshold: 0.3
        };
        
        // Market data
        this.markets = [
            { name: 'US_SP500', type: 'developed', region: 'North America' },
            { name: 'US_NASDAQ', type: 'developed', region: 'North America' },
            { name: 'US_RUSSELL', type: 'developed', region: 'North America' },
            { name: 'EU_STOXX', type: 'developed', region: 'Europe' },
            { name: 'DE_DAX', type: 'developed', region: 'Europe' },
            { name: 'FR_CAC40', type: 'developed', region: 'Europe' },
            { name: 'UK_FTSE', type: 'developed', region: 'Europe' },
            { name: 'JP_NIKKEI', type: 'developed', region: 'Asia' },
            { name: 'JP_TOPIX', type: 'developed', region: 'Asia' },
            { name: 'AU_ASX', type: 'developed', region: 'Asia Pacific' },
            { name: 'CA_TSX', type: 'developed', region: 'North America' },
            { name: 'CH_SMI', type: 'developed', region: 'Europe' },
            { name: 'CN_SHANGHAI', type: 'emerging', region: 'Asia' },
            { name: 'CN_CSI300', type: 'emerging', region: 'Asia' },
            { name: 'HK_HANGSENG', type: 'emerging', region: 'Asia' },
            { name: 'IN_SENSEX', type: 'emerging', region: 'Asia' },
            { name: 'IN_NIFTY', type: 'emerging', region: 'Asia' },
            { name: 'BR_BOVESPA', type: 'emerging', region: 'South America' },
            { name: 'ZA_JSE', type: 'emerging', region: 'Africa' },
            { name: 'RU_MOEX', type: 'emerging', region: 'Europe' },
            { name: 'KR_KOSPI', type: 'emerging', region: 'Asia' },
            { name: 'TW_TAIEX', type: 'emerging', region: 'Asia' },
            { name: 'MX_IPC', type: 'emerging', region: 'North America' },
            { name: 'TR_BIST', type: 'emerging', region: 'Europe' }
        ];
    }
    
    initialize() {
        if (!window.THREE) {
            throw new Error('Three.js is required for 3D visualization');
        }
        
        this.createScene();
        this.createCamera();
        this.createRenderer();
        this.createControls();
        this.createLights();
        
        this.generateNetworkData();
        this.createNetworkObjects();
        this.positionNodes();
        
        this.startAnimation();
        this.setupEventListeners();
        
        this.isInitialized = true;
        console.log('3D Network visualization initialized');
    }
    
    createScene() {
        this.scene = new THREE.Scene();
        this.scene.background = new THREE.Color(0x0a0b0f);
        
        // Add fog for depth perception
        this.scene.fog = new THREE.Fog(0x0a0b0f, 50, 200);
    }
    
    createCamera() {
        const aspect = this.container.offsetWidth / this.container.offsetHeight;
        this.camera = new THREE.PerspectiveCamera(75, aspect, 0.1, 1000);
        this.camera.position.set(30, 20, 30);
        this.camera.lookAt(0, 0, 0);
    }
    
    createRenderer() {
        this.renderer = new THREE.WebGLRenderer({ 
            antialias: true,
            alpha: true
        });
        this.renderer.setSize(this.container.offsetWidth, this.container.offsetHeight);
        this.renderer.setPixelRatio(window.devicePixelRatio);
        this.renderer.shadowMap.enabled = true;
        this.renderer.shadowMap.type = THREE.PCFSoftShadowMap;
        
        this.container.appendChild(this.renderer.domElement);
    }
    
    createControls() {
        // OrbitControls would be imported in a real implementation
        // For now, we'll create a simple mouse control
        this.setupMouseControls();
    }
    
    setupMouseControls() {
        let isMouseDown = false;
        let mouseX = 0;
        let mouseY = 0;
        let targetRotationX = 0;
        let targetRotationY = 0;
        let rotationX = 0;
        let rotationY = 0;
        
        this.renderer.domElement.addEventListener('mousedown', (e) => {
            isMouseDown = true;
            mouseX = e.clientX;
            mouseY = e.clientY;
        });
        
        this.renderer.domElement.addEventListener('mousemove', (e) => {
            if (!isMouseDown) return;
            
            const deltaX = e.clientX - mouseX;
            const deltaY = e.clientY - mouseY;
            
            targetRotationY += deltaX * 0.01;
            targetRotationX += deltaY * 0.01;
            
            mouseX = e.clientX;
            mouseY = e.clientY;
        });
        
        this.renderer.domElement.addEventListener('mouseup', () => {
            isMouseDown = false;
        });
        
        this.renderer.domElement.addEventListener('wheel', (e) => {
            const delta = e.deltaY * 0.05;
            this.camera.position.multiplyScalar(1 + delta * 0.1);
        });
        
        // Update camera rotation in animation loop
        this.updateCameraRotation = () => {
            rotationX += (targetRotationX - rotationX) * 0.05;
            rotationY += (targetRotationY - rotationY) * 0.05;
            
            const radius = this.camera.position.length();
            this.camera.position.x = radius * Math.sin(rotationY) * Math.cos(rotationX);
            this.camera.position.y = radius * Math.sin(rotationX);
            this.camera.position.z = radius * Math.cos(rotationY) * Math.cos(rotationX);
            this.camera.lookAt(0, 0, 0);
        };
    }
    
    createLights() {
        // Ambient light
        const ambientLight = new THREE.AmbientLight(0x64ffda, 0.4);
        this.scene.add(ambientLight);
        
        // Directional light
        const directionalLight = new THREE.DirectionalLight(0x00e5ff, 0.8);
        directionalLight.position.set(50, 50, 25);
        directionalLight.castShadow = true;
        directionalLight.shadow.mapSize.width = 2048;
        directionalLight.shadow.mapSize.height = 2048;
        this.scene.add(directionalLight);
        
        // Point lights for atmosphere
        const pointLight1 = new THREE.PointLight(0x1de9b6, 0.5, 100);
        pointLight1.position.set(25, 25, 25);
        this.scene.add(pointLight1);
        
        const pointLight2 = new THREE.PointLight(0xffc947, 0.3, 100);
        pointLight2.position.set(-25, -25, -25);
        this.scene.add(pointLight2);
    }
    
    generateNetworkData() {
        // Generate nodes
        this.nodes = this.markets.map((market, index) => ({
            id: index,
            name: market.name,
            type: market.type,
            region: market.region,
            size: Math.random() * 0.5 + 0.5,
            centrality: Math.random(),
            volume: Math.random(),
            volatility: Math.random(),
            position: new THREE.Vector3(),
            velocity: new THREE.Vector3(),
            force: new THREE.Vector3()
        }));
        
        // Generate edges based on correlation/transfer entropy
        this.edges = [];
        for (let i = 0; i < this.nodes.length; i++) {
            for (let j = i + 1; j < this.nodes.length; j++) {
                const strength = this.calculateConnectionStrength(this.nodes[i], this.nodes[j]);
                
                if (strength > this.config.threshold) {
                    this.edges.push({
                        source: i,
                        target: j,
                        strength: strength,
                        weight: strength * 5
                    });
                }
            }
        }
        
        console.log(`Generated ${this.nodes.length} nodes and ${this.edges.length} edges`);
    }
    
    calculateConnectionStrength(node1, node2) {
        let baseStrength = Math.random() * 0.8 + 0.1;
        
        // Higher correlation within same type
        if (node1.type === node2.type) {
            baseStrength *= 1.5;
        }
        
        // Higher correlation within same region
        if (node1.region === node2.region) {
            baseStrength *= 1.3;
        }
        
        // Add some noise
        baseStrength += (Math.random() - 0.5) * 0.2;
        
        return Math.max(0, Math.min(1, baseStrength));
    }
    
    createNetworkObjects() {
        this.createNodeObjects();
        this.createEdgeObjects();
        this.createLabels();
    }
    
    createNodeObjects() {
        this.nodeObjects = [];
        
        this.nodes.forEach((node, index) => {
            // Create node geometry and material
            const geometry = new THREE.SphereGeometry(this.getNodeSize(node), 32, 32);
            const material = new THREE.MeshPhongMaterial({
                color: this.getNodeColor(node),
                transparent: true,
                opacity: 0.8,
                shininess: 100
            });
            
            const mesh = new THREE.Mesh(geometry, material);
            mesh.userData = { nodeIndex: index, type: 'node' };
            mesh.castShadow = true;
            mesh.receiveShadow = true;
            
            // Add glow effect
            const glowGeometry = new THREE.SphereGeometry(this.getNodeSize(node) * 1.5, 16, 16);
            const glowMaterial = new THREE.MeshBasicMaterial({
                color: this.getNodeColor(node),
                transparent: true,
                opacity: 0.2,
                side: THREE.BackSide
            });
            const glow = new THREE.Mesh(glowGeometry, glowMaterial);
            mesh.add(glow);
            
            this.scene.add(mesh);
            this.nodeObjects.push(mesh);
        });
    }
    
    createEdgeObjects() {
        this.edgeObjects = [];
        
        this.edges.forEach(edge => {
            if (edge.strength < this.config.threshold) return;
            
            const sourcePos = this.nodes[edge.source].position;
            const targetPos = this.nodes[edge.target].position;
            
            // Create edge geometry
            const geometry = new THREE.BufferGeometry();
            const positions = new Float32Array([
                sourcePos.x, sourcePos.y, sourcePos.z,
                targetPos.x, targetPos.y, targetPos.z
            ]);
            geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
            
            // Create edge material
            const material = new THREE.LineBasicMaterial({
                color: this.config.colors.edge,
                transparent: true,
                opacity: this.getEdgeOpacity(edge)
            });
            
            const line = new THREE.Line(geometry, material);
            line.userData = { edgeIndex: this.edgeObjects.length, type: 'edge' };
            
            this.scene.add(line);
            this.edgeObjects.push(line);
        });
    }
    
    createLabels() {
        // In a full implementation, we would create text sprites for node labels
        // For now, we'll skip labels to keep the code simple
    }
    
    getNodeSize(node) {
        const baseSize = this.config.nodeSize.min;
        const sizeRange = this.config.nodeSize.max - this.config.nodeSize.min;
        
        switch (this.config.nodeSize.metric || 'centrality') {
            case 'centrality':
                return baseSize + node.centrality * sizeRange;
            case 'volume':
                return baseSize + node.volume * sizeRange;
            case 'volatility':
                return baseSize + node.volatility * sizeRange;
            default:
                return baseSize + sizeRange * 0.5;
        }
    }
    
    getNodeColor(node) {
        if (node.centrality > 0.8) {
            return this.config.colors.central;
        }
        return node.type === 'developed' ? 
            this.config.colors.developed : 
            this.config.colors.emerging;
    }
    
    getEdgeOpacity(edge) {
        const baseOpacity = this.config.edgeOpacity.min;
        const opacityRange = this.config.edgeOpacity.max - this.config.edgeOpacity.min;
        return baseOpacity + edge.strength * opacityRange;
    }
    
    positionNodes() {
        switch (this.config.layout) {
            case 'force':
                this.forceDirectedLayout();
                break;
            case 'circular':
                this.circularLayout();
                break;
            case 'hierarchical':
                this.hierarchicalLayout();
                break;
            case 'geographic':
                this.geographicLayout();
                break;
            default:
                this.forceDirectedLayout();
        }
        
        this.updateNodePositions();
    }
    
    forceDirectedLayout() {
        // Simple force-directed layout algorithm
        const iterations = 100;
        const k = Math.sqrt(400 / this.nodes.length); // Optimal distance
        const c = 0.01; // Cooling factor
        
        // Initialize random positions
        this.nodes.forEach(node => {
            node.position.set(
                (Math.random() - 0.5) * 40,
                (Math.random() - 0.5) * 40,
                (Math.random() - 0.5) * 40
            );
            node.velocity.set(0, 0, 0);
        });
        
        // Run force simulation
        for (let iter = 0; iter < iterations; iter++) {
            // Reset forces
            this.nodes.forEach(node => {
                node.force.set(0, 0, 0);
            });
            
            // Repulsive forces between all nodes
            for (let i = 0; i < this.nodes.length; i++) {
                for (let j = i + 1; j < this.nodes.length; j++) {
                    const node1 = this.nodes[i];
                    const node2 = this.nodes[j];
                    
                    const dx = node1.position.x - node2.position.x;
                    const dy = node1.position.y - node2.position.y;
                    const dz = node1.position.z - node2.position.z;
                    
                    const distance = Math.sqrt(dx * dx + dy * dy + dz * dz);
                    if (distance > 0) {
                        const repulsion = k * k / distance;
                        const fx = (dx / distance) * repulsion;
                        const fy = (dy / distance) * repulsion;
                        const fz = (dz / distance) * repulsion;
                        
                        node1.force.x += fx;
                        node1.force.y += fy;
                        node1.force.z += fz;
                        node2.force.x -= fx;
                        node2.force.y -= fy;
                        node2.force.z -= fz;
                    }
                }
            }
            
            // Attractive forces along edges
            this.edges.forEach(edge => {
                const node1 = this.nodes[edge.source];
                const node2 = this.nodes[edge.target];
                
                const dx = node2.position.x - node1.position.x;
                const dy = node2.position.y - node1.position.y;
                const dz = node2.position.z - node1.position.z;
                
                const distance = Math.sqrt(dx * dx + dy * dy + dz * dz);
                if (distance > 0) {
                    const attraction = (distance * distance) / k * edge.strength;
                    const fx = (dx / distance) * attraction;
                    const fy = (dy / distance) * attraction;
                    const fz = (dz / distance) * attraction;
                    
                    node1.force.x += fx;
                    node1.force.y += fy;
                    node1.force.z += fz;
                    node2.force.x -= fx;
                    node2.force.y -= fy;
                    node2.force.z -= fz;
                }
            });
            
            // Update positions
            const temperature = c * (1 - iter / iterations);
            this.nodes.forEach(node => {
                const forceLength = Math.sqrt(
                    node.force.x * node.force.x + 
                    node.force.y * node.force.y + 
                    node.force.z * node.force.z
                );
                
                if (forceLength > 0) {
                    const displacement = Math.min(forceLength, temperature);
                    node.position.x += (node.force.x / forceLength) * displacement;
                    node.position.y += (node.force.y / forceLength) * displacement;
                    node.position.z += (node.force.z / forceLength) * displacement;
                }
            });
        }
    }
    
    circularLayout() {
        const radius = 20;
        const angleStep = (Math.PI * 2) / this.nodes.length;
        
        this.nodes.forEach((node, index) => {
            const angle = index * angleStep;
            node.position.set(
                radius * Math.cos(angle),
                0,
                radius * Math.sin(angle)
            );
        });
    }
    
    hierarchicalLayout() {
        // Group by type and arrange in layers
        const developed = this.nodes.filter(n => n.type === 'developed');
        const emerging = this.nodes.filter(n => n.type === 'emerging');
        
        // Arrange developed markets in inner circle
        const innerRadius = 15;
        developed.forEach((node, index) => {
            const angle = (index / developed.length) * Math.PI * 2;
            node.position.set(
                innerRadius * Math.cos(angle),
                5,
                innerRadius * Math.sin(angle)
            );
        });
        
        // Arrange emerging markets in outer circle
        const outerRadius = 30;
        emerging.forEach((node, index) => {
            const angle = (index / emerging.length) * Math.PI * 2;
            node.position.set(
                outerRadius * Math.cos(angle),
                -5,
                outerRadius * Math.sin(angle)
            );
        });
    }
    
    geographicLayout() {
        // Arrange nodes based on geographic regions
        const regionPositions = {
            'North America': { x: -20, y: 0, z: 10 },
            'Europe': { x: 0, y: 0, z: 0 },
            'Asia': { x: 20, y: 0, z: 0 },
            'Asia Pacific': { x: 25, y: 0, z: -10 },
            'South America': { x: -15, y: -10, z: 20 },
            'Africa': { x: 5, y: -10, z: 15 }
        };
        
        const regionCounts = {};
        this.nodes.forEach(node => {
            const region = node.region;
            if (!regionCounts[region]) regionCounts[region] = 0;
            
            const basePos = regionPositions[region] || { x: 0, y: 0, z: 0 };
            const offset = regionCounts[region] * 3;
            const angle = (regionCounts[region] / 5) * Math.PI * 2;
            
            node.position.set(
                basePos.x + Math.cos(angle) * offset,
                basePos.y,
                basePos.z + Math.sin(angle) * offset
            );
            
            regionCounts[region]++;
        });
    }
    
    updateNodePositions() {
        this.nodeObjects.forEach((mesh, index) => {
            const node = this.nodes[index];
            mesh.position.copy(node.position);
        });
        
        this.updateEdgePositions();
    }
    
    updateEdgePositions() {
        this.edgeObjects.forEach((line, index) => {
            const edge = this.edges[index];
            const sourcePos = this.nodes[edge.source].position;
            const targetPos = this.nodes[edge.target].position;
            
            const positions = line.geometry.attributes.position.array;
            positions[0] = sourcePos.x;
            positions[1] = sourcePos.y;
            positions[2] = sourcePos.z;
            positions[3] = targetPos.x;
            positions[4] = targetPos.y;
            positions[5] = targetPos.z;
            
            line.geometry.attributes.position.needsUpdate = true;
        });
    }
    
    startAnimation() {
        this.animate();
    }
    
    animate() {
        this.animationFrame = requestAnimationFrame(() => this.animate());
        
        // Update camera controls
        if (this.updateCameraRotation) {
            this.updateCameraRotation();
        }
        
        // Animate nodes (gentle floating motion)
        const time = Date.now() * 0.001;
        this.nodeObjects.forEach((mesh, index) => {
            const node = this.nodes[index];
            mesh.position.y = node.position.y + Math.sin(time + index) * 0.5;
            mesh.rotation.y += 0.01;
        });
        
        // Animate edge opacity
        this.edgeObjects.forEach((line, index) => {
            const edge = this.edges[index];
            const baseOpacity = this.getEdgeOpacity(edge);
            const animatedOpacity = baseOpacity + Math.sin(time * 2 + index) * 0.1;
            line.material.opacity = Math.max(0.1, animatedOpacity);
        });
        
        this.renderer.render(this.scene, this.camera);
    }
    
    setupEventListeners() {
        // Mouse events for interaction
        const raycaster = new THREE.Raycaster();
        const mouse = new THREE.Vector2();
        
        this.renderer.domElement.addEventListener('click', (event) => {
            mouse.x = ((event.clientX - this.container.offsetLeft) / this.container.offsetWidth) * 2 - 1;
            mouse.y = -((event.clientY - this.container.offsetTop) / this.container.offsetHeight) * 2 + 1;
            
            raycaster.setFromCamera(mouse, this.camera);
            const intersects = raycaster.intersectObjects(this.nodeObjects);
            
            if (intersects.length > 0) {
                const nodeIndex = intersects[0].object.userData.nodeIndex;
                this.highlightNode(nodeIndex);
            }
        });
    }
    
    highlightNode(nodeIndex) {
        // Reset all nodes
        this.nodeObjects.forEach(mesh => {
            mesh.material.emissive.setHex(0x000000);
            mesh.scale.set(1, 1, 1);
        });
        
        // Highlight selected node
        const selectedMesh = this.nodeObjects[nodeIndex];
        selectedMesh.material.emissive.setHex(0x444444);
        selectedMesh.scale.set(1.2, 1.2, 1.2);
        
        // Highlight connected edges
        this.edgeObjects.forEach((line, index) => {
            const edge = this.edges[index];
            if (edge.source === nodeIndex || edge.target === nodeIndex) {
                line.material.opacity = 0.8;
                line.material.color.setHex(0xffc947);
            } else {
                line.material.opacity = 0.1;
                line.material.color.setHex(0x1de9b6);
            }
        });
        
        console.log('Node selected:', this.nodes[nodeIndex].name);
    }
    
    // Public methods for external control
    setScale(scale) {
        this.config.scale = scale;
        this.updateData();
    }
    
    setThreshold(threshold) {
        this.config.threshold = threshold;
        this.rebuildEdges();
    }
    
    setLayout(layout) {
        this.config.layout = layout;
        this.positionNodes();
    }
    
    setNodeSize(metric) {
        this.config.nodeSize.metric = metric;
        this.updateNodeSizes();
    }
    
    updateData() {
        // Simulate data update
        this.nodes.forEach(node => {
            node.centrality = Math.random();
            node.volume = Math.random();
            node.volatility = Math.random();
        });
        
        this.updateNodeSizes();
        this.updateNodeColors();
        this.rebuildEdges();
    }
    
    updateNodeSizes() {
        this.nodeObjects.forEach((mesh, index) => {
            const node = this.nodes[index];
            const newSize = this.getNodeSize(node);
            mesh.scale.setScalar(newSize / this.config.nodeSize.min);
        });
    }
    
    updateNodeColors() {
        this.nodeObjects.forEach((mesh, index) => {
            const node = this.nodes[index];
            mesh.material.color.setHex(this.getNodeColor(node));
        });
    }
    
    rebuildEdges() {
        // Remove existing edges
        this.edgeObjects.forEach(line => {
            this.scene.remove(line);
            line.geometry.dispose();
            line.material.dispose();
        });
        this.edgeObjects = [];
        
        // Rebuild edges with new threshold
        this.edges = [];
        for (let i = 0; i < this.nodes.length; i++) {
            for (let j = i + 1; j < this.nodes.length; j++) {
                const strength = this.calculateConnectionStrength(this.nodes[i], this.nodes[j]);
                
                if (strength > this.config.threshold) {
                    this.edges.push({
                        source: i,
                        target: j,
                        strength: strength,
                        weight: strength * 5
                    });
                }
            }
        }
        
        this.createEdgeObjects();
        this.updateNetworkStats();
    }
    
    updateNetworkStats() {
        // Update network statistics display
        const nodeCount = document.getElementById('node-count');
        const edgeCount = document.getElementById('edge-count');
        const networkDensity = document.getElementById('network-density');
        const clusteringCoeff = document.getElementById('clustering-coeff');
        
        if (nodeCount) nodeCount.textContent = this.nodes.length;
        if (edgeCount) edgeCount.textContent = this.edges.length;
        
        const maxEdges = (this.nodes.length * (this.nodes.length - 1)) / 2;
        const density = this.edges.length / maxEdges;
        if (networkDensity) networkDensity.textContent = density.toFixed(2);
        
        // Simple clustering coefficient calculation
        const clustering = this.calculateClusteringCoefficient();
        if (clusteringCoeff) clusteringCoeff.textContent = clustering.toFixed(2);
    }
    
    calculateClusteringCoefficient() {
        // Simplified clustering coefficient calculation
        let totalClustering = 0;
        
        this.nodes.forEach((node, i) => {
            const neighbors = this.getNeighbors(i);
            if (neighbors.length < 2) return;
            
            let connectedPairs = 0;
            for (let j = 0; j < neighbors.length; j++) {
                for (let k = j + 1; k < neighbors.length; k++) {
                    if (this.areConnected(neighbors[j], neighbors[k])) {
                        connectedPairs++;
                    }
                }
            }
            
            const possiblePairs = (neighbors.length * (neighbors.length - 1)) / 2;
            const clustering = connectedPairs / possiblePairs;
            totalClustering += clustering;
        });
        
        return totalClustering / this.nodes.length;
    }
    
    getNeighbors(nodeIndex) {
        const neighbors = [];
        this.edges.forEach(edge => {
            if (edge.source === nodeIndex) {
                neighbors.push(edge.target);
            } else if (edge.target === nodeIndex) {
                neighbors.push(edge.source);
            }
        });
        return neighbors;
    }
    
    areConnected(node1, node2) {
        return this.edges.some(edge => 
            (edge.source === node1 && edge.target === node2) ||
            (edge.source === node2 && edge.target === node1)
        );
    }
    
    handleResize() {
        if (!this.isInitialized) return;
        
        const width = this.container.offsetWidth;
        const height = this.container.offsetHeight;
        
        this.camera.aspect = width / height;
        this.camera.updateProjectionMatrix();
        
        this.renderer.setSize(width, height);
    }
    
    destroy() {
        if (this.animationFrame) {
            cancelAnimationFrame(this.animationFrame);
        }
        
        // Dispose of geometries and materials
        this.nodeObjects.forEach(mesh => {
            mesh.geometry.dispose();
            mesh.material.dispose();
            this.scene.remove(mesh);
        });
        
        this.edgeObjects.forEach(line => {
            line.geometry.dispose();
            line.material.dispose();
            this.scene.remove(line);
        });
        
        // Remove renderer from DOM
        if (this.renderer && this.container.contains(this.renderer.domElement)) {
            this.container.removeChild(this.renderer.domElement);
        }
        
        // Dispose of renderer
        if (this.renderer) {
            this.renderer.dispose();
        }
        
        console.log('3D Network visualization destroyed');
    }
}

// Export for use in main application
window.Network3D = Network3D;